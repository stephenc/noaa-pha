# TOB History Reconstruction Algorithm: Forward Scanning

## Overview

This algorithm reconstructs Time-of-Observation (TOB) histories by **growing segments forward** until perfect fit breaks, allowing boundaries to emerge naturally from the data rather than relying on split detection heuristics.

## Scope: TOB + PHA Boundary Detection

This algorithm detects boundaries at both TOB regime changes and PHA step changes, then filters output to include only TOB-related transitions in the `.his` file.

**Why detect both?**
- The residual R(t) = QCF(t) - QCU(t) = TOB(t) + PHA(t) contains both signals
- A perfect fit (≤3 distinct residual values) requires both TOB and PHA to be stable
- When either changes, perfect fit breaks

**Boundary types:**
- **TOB change**: Monthly observation time pattern shifts (e.g., 7AM → 5PM)
- **PHA step**: Constant offset applied across all months (homogenization adjustment)

**Output filtering:**
The `.his` file contains only TOB-related boundaries:
- **TOB-only change**: Written to `.his` with new TOB code
- **PHA-only change**: Excluded from `.his` (would seed PHA algorithm with false TOB transitions)
- **TOB+PHA together**: Written to `.his` with new TOB code

## Core Concept

**Key Insight**: A perfect TOB fit means residuals have ≤3 distinct values (when rounded to 0.01°C). We can track which TOB codes maintain this property as we scan forward through time. When no codes remain valid, we've found a boundary.

**Algorithm Flow**:
1. Generate basis vectors for all ~30 TOB codes once per station
2. Start with first 12-month window, track which codes have ≤3 distinct residual values
3. Step forward one month at a time
4. Eliminate codes whose distinct value count exceeds 3
5. When NO codes remain valid → current month starts new segment
6. Refine boundaries and validate timing (month and day-level)
7. Detect and fix pathological segmentation
8. Recode short bridge segments that a neighbour's code fits better
9. Cross-gap attribution: extend far-side code over PHA-continuous bridges
10. Merge same-code segments

## Detailed Algorithm

### Phase 1: Basis Vector Generation

For each TOB code (07HR, 17HR, 24HR, etc.), generate a basis vector showing the expected TOB adjustment for each month. This is done by running the TOB model with that code to produce adjusted temperatures, then computing the adjustment pattern.

The inventory lat/lon is used for all periods. Pass `--mshr-tob-lat-lon` to instead encode the full MSHR location history in each synthetic `.his` (one row per MSHR period, same TOB code throughout), causing TOBMain to use the per-period lat/lon when computing expected adjustments. Where no MSHR record exists or coordinates are absent the inventory lat/lon is used as a fallback. The validation run uses the same lat/lon mode as basis vector generation.

**Cost**: O(30 × T) where T is number of months — all codes run in a single TOBMain batch per station.

**Output**: A dictionary mapping each TOB code to its monthly adjustment pattern.

### Phase 2: Forward Scanning

**Initialization**:
- Start with window beginning at first data month
- All ~30 TOB codes are initially candidate codes
- Track the set of valid codes (those with ≤3 distinct residual values)

**Month-by-month scanning**:

For each month with data:

1. **Gap detection**: If ≥12 months since last data, force a boundary (likely station move or equipment change)

2. **Residual computation**: For each candidate code:
   - Subtract the code's basis vector from QCF-QCU residuals
   - Count distinct values in the growing window
   - Use integer arithmetic (cents) to avoid floating-point errors

3. **Code elimination**: Remove codes exceeding 3 distinct values

4. **Boundary detection**: When no codes remain valid:
   - Save the last valid code set (from previous month)
   - Create a segment ending at the refined boundary
   - Reset to new window starting at current month

5. **Walk-back check**: If gap between segments ≥18 months, invoke walk-back to split TOB+PHA transitions

**Key principle**: Segments grow naturally until perfect fit breaks, rather than pre-computing boundaries.

### Boundary Refinement (within Phase 2)

When a code is eliminated (distinct values exceed 3), the detection point may lag the actual transition. Refinement scans backwards to find where the residual distribution actually shifted.

**Approach**:
- Look backwards from detection point
- Find where residual set changed from one stable distribution to another
- Check for disjoint sets or significant variance changes
- Typically adjusts boundaries by 1-6 months

**Why needed**: The transition month may have intermediate values that don't break the ≤3 threshold immediately, causing delayed detection.

### Phase 3: Boundary Timing Validation

After forward scan completes, refine each boundary by testing adjustments of ±1 through ±6 months to minimize total distinct values across both adjacent segments.

**Algorithm**:
```
For each boundary between segments A and B:
  current_total = distinct_values(A) + distinct_values(B)
  best_total = current_total
  best_boundary = current_boundary

  For shift in [-1, +1, -2, +2, -3, +3, -4, +4, -5, +5, -6, +6]:
    A' = segment A with adjusted end
    B' = segment B with adjusted start
    new_total = distinct_values(A') + distinct_values(B')

    If new_total < best_total:
      best_total = new_total
      best_boundary = current_boundary + shift

  Update boundary to best_boundary
```

Shifts are tried in order of increasing absolute magnitude. When multiple shifts achieve the same total distinct count, the smallest shift (closest to the detected boundary) is preferred, since a later equal shift cannot displace an already-accepted one. A larger shift can still win only if it strictly reduces the total further — which happens when, for example, a missing data month immediately before the true transition causes the scan to land several months late.

**Key principle**: Even when both segments individually achieve "perfect fit" (≤3 distinct values), prefer boundaries that minimize total distinct values. For example, 1+3=4 is better than 3+3=6, even though both configurations have perfect fit on each side.

**Adaptive minimum segment length**: The shift search is constrained so neither segment shrinks below a minimum. For normal-length segments this minimum is `MIN_SEGMENT_MONTHS` (10). For short segments — e.g. the first post-gap window, which may be only a few months — the minimum scales down to `max(2, span // 2)` where `span = end_month - start_month`. This prevents a short post-gap segment from blocking all backward shifts and thereby stranding the true transition boundary several months inside the wrong regime.

**PHA-only detection**: If after adjustment, both segments use the same TOB code and both have perfect fit, this indicates a PHA-only step (constant offset, not TOB change). Mark the second segment as `include_in_his=False` to exclude from output.

### Phase 4: Spike Boundary Refinement (Day-Level)

For code-changing boundaries where the transition month contains an out-of-bound residual spike affecting both codes, search within the month (days 2-31) to find the optimal day that minimizes the spike artifact.

**When applied**: Only at boundaries where:
1. TOB code changes (not PHA-only transitions)
2. Transition month residual is outside expected range for BOTH codes (±0.01°C tolerance from ±6 month window)

Two check directions are performed for each boundary:

- **Forward spike** (existing): the **first** month of the new segment is the suspected transition month. Applied when validate_boundary_timing placed the boundary at month T but month T's residual doesn't fit either adjacent code.
- **Backward spike** (new): the **last** month of the old segment is the suspected transition month. Applied when the true regime change happened mid-month within `prev_seg.end_month` rather than at the start of the new segment. The backward check fires first; if it triggers, the forward check is skipped for that boundary.

**Algorithm**:
```
For each qualifying boundary:
  For split_day in [2, 3, ..., days_in_month]:
    weighted_residual = (days_before × residual_prev_code +
                         days_after × residual_curr_code) / days_in_month

    Count distinct values in ±3 month window with weighted transition
    Calculate distance from midpoint of both codes' ranges

    Score: Prefer 2 distinct values, then minimize midpoint distance

  Apply best split day if better than month boundary
```

**Example**: Station with spike at May 2009 where residual is anomalous for both 14HR and 24HR codes:
- Test splits: May 1-2, May 2-3, ..., May 30-31
- Calculate weighted average for each: `(day-1) × 14HR_residual + (31-day+1) × 24HR_residual) / 31`
- Select day that produces fewest distinct values or gets closest to expected range

**Output**: Segments with `start_day` and `end_day` fields set (e.g., 14HR ends May 15, 24HR starts May 16). TOBMain applies weighted TOB adjustment based on days within each code during the month.

**Why needed**: Some transitions produce single-month residual spikes that fall outside the normal range for both codes. Month-level boundaries would force the entire month into one code, preserving the spike. Day-level splits allow TOBMain to apply weighted averaging, potentially reducing the spike to within expected ranges.

### Phase 5: Pathological Segmentation Detection

When forward scan produces segments changing more frequently than every 12 months, this indicates fitting to seasonal noise rather than actual TOB regime changes.

**Detection criterion**:
- Count segments per calendar year
- If ≥3 years have >1 segment boundary, mark as pathological pattern
- This indicates the algorithm is fitting to repeating seasonal noise

#### Growing Pathological Regions

Once pathological segments are identified, grow regions by matching monthly residual patterns:

**Pattern matching algorithm**:

1. For each pathological segment, build a monthly pattern (one residual value per calendar month 1-12)

2. Grow forward:
   - For each subsequent segment, check if residuals match the pattern
   - Calendar months already seen must match within ±0.01°C
   - New calendar months get added to the pattern
   - Stop when pattern breaks (indicates PHA boundary)

3. Grow backward (same logic in reverse)

4. Find best-variance TOB code for the entire region

5. Replace all segments in the region with a single merged segment

**Key insight**: Pattern matching with ±0.01°C tolerance allows growing across the same TOB regime while stopping at PHA boundaries where residuals shift by ~0.5-1.0°C.

#### Handling Gaps Between Pathological Regions

After growing regions, gaps may exist between adjacent pathological regions. When both regions have the same TOB code, the gap represents a PHA-only transition.

**Gap handling**:
- If two pathological regions with same code separated by gap < 12 months
- Mark gap segment(s) as `include_in_his=False` (PHA-only)
- Change gap's TOB code to match adjacent regions
- Merge phase will combine all three into one segment

**Why this matters**: Prevents suggesting TOB changes at PHA boundaries. When the same TOB code fits both sides of a PHA step, output shows one merged segment spanning the entire period.

**Example**: Station with PHA step at 1975-07 where residuals jump from 0.3°C to -0.5°C:
- Pathological detection finds two regions: 1959-09 to 1975-05 and 1975-12 to 1984-12
- Both regions independently determine 16HR is best fit
- Gap (1975-06 to 1975-11) marked as PHA-only
- Output: Single 16HR segment from 1959-09 to 1984-12

#### Cleanup Operations

Before processing pathological regions:
- Drop regions < 12 months (too short to reliably assess if pathological)
- Merge overlapping regions: two grown regions can overlap (share segment indices) when different seed pathological segments independently grow into the same territory. Overlapping regions are merged into a single region covering the union of their segment indices, then reassigned the best-variance code for that combined span.
- Refine boundaries if needed for better fit

### Phase 6: Short Bridge Segment Recoding

Short segments (fewer than `MIN_SEGMENT_MONTHS` data points) can arise when the forward scan traverses a data gap. The few data points in the bridge may be consistent with several codes, and the chosen code may differ from the dominant neighbouring regime.

**Algorithm**:

```
For each segment S with data_point_count(S) < MIN_SEGMENT_MONTHS:
  If S is excluded from .his (PHA-only), skip
  Build candidate set: {prev_code, next_code} from adjacent include_in_his segments
  Compute distinct-value count for S under S's current code
  Compute distinct-value count for S under each candidate code

  If any candidate gives strictly fewer distinct values:
    Recode S to the best candidate
    (Phase 8 will then merge S with the same-code neighbour)
  Elif a candidate gives equal distinct values AND that code appears on BOTH sides:
    Recode S to that code (eliminates the bridge entirely after merging)
  Else:
    Leave S unchanged (current code is already optimal given the data)

Repeat until no further recodes occur.
```

**Key constraint**: Only recodes when a neighbour's code gives a *strictly* better fit. If the current code is tied with or better than both neighbours — which happens when the bridge data was genuinely observed under that code — the bridge is left unchanged.

**Example**: A 6-month `24HR` bridge between `07HR` and `00SS`:
- If `24HR` gives 3 distinct values, `00SS` gives 5, `07HR` gives 6 → no recode (24HR is best)
- If `24HR` gives 3 distinct values, `00SS` gives 1 → recode to `00SS`, enabling merge with the long `00SS` successor

### Phase 7: Cross-Gap TOB Attribution

When the forward scan crosses a data gap, a short bridge segment may be assigned the **near-side TOB code** (the same code as the immediately preceding segment) even though the data in the bridge is equally consistent with the **far-side code** (the code of the next include_in_his segment). This misattribution happens because the forward scan begins each gap by continuing from the last known code.

The cross-gap attribution phase corrects this by testing whether the PHA adjustment is continuous across the gap.

**Algorithm**:

```
For each segment B with data_point_count(B) < MIN_SEGMENT_MONTHS AND include_in_his=False:
  near_code = B.tob_code
  prev_his_seg = most recent include_in_his=True segment before B
  If prev_his_seg.tob_code ≠ near_code: skip  (bridge code must match near-side)

  far_seg = next include_in_his=True segment after B
  If far_seg.tob_code == near_code: skip  (no TOB change to attribute)
  far_code = far_seg.tob_code

  # Collect residuals from each side of the gap
  pre_cents  = { round((R(t) - near_basis(t)) * 100) : t ∈ prev_his_seg, last 36 months }
  post_cents = { round((R(t) - far_basis(t))  * 100) : t ∈ far_seg,      first 36 months }

  # PHA-continuity test
  If len(pre_cents | post_cents) > 3: skip  (PHA step across gap → keep original boundary)

  # Bridge consistency test
  bridge_distinct = count_distinct(R(t) - far_basis(t) for t ∈ B)
  If bridge_distinct > 3: skip

  # Tests passed: bridge belongs to the far-side regime
  Recode B to far_code, set include_in_his = True
  (Phase 8 merge_segments will then fold B into far_seg if no MSHR separates them)
```

**Why this works**: R(t) = PHA(t) + TOB_code(t). If `pre_cents ∪ post_cents` has ≤ 3 distinct values, the PHA component is constant across the gap under the respective code bases — meaning there is no PHA step at the gap boundary. The TOB boundary belongs on the far side of the gap, not at the data edge closest to the near side.

### Phase 8: Segment Merging

After all boundary refinement and bridge recoding, merge consecutive segments with the same TOB code.

**Merging rules**:
- Combine if same code and no documented MSHR record between them
- MSHR records indicate known station moves or equipment changes
- Segments marked `include_in_his=False` can merge with adjacent TOB segments
- Merged segment has `include_in_his=True` if either component had it

**Purpose**: Ensures PHA-only transitions don't break up same-code TOB runs in the output.

### Output: .his File Generation

The `.his` file is built from the final set of segments plus all MSHR records for the station.

**Row structure**: Each output row spans a contiguous date range and carries a single TOB code. Multiple rows are produced per TOB segment when MSHR records begin within or after that segment's date range.

**MSHR split points**: For every MSHR record whose `begin_date` falls within a TOB segment's range, an additional row begins at that MSHR `begin_date` (same TOB code, updated metadata from the new MSHR record).

**Trailing MSHR records**: MSHR records beginning after the last TOB segment's end produce additional rows, carrying the last known TOB code. This ensures the `.his` file covers the full span of documented station history even beyond the data used for reconstruction.

**Contiguous dates**: Each row's `end_date` is derived as the day before the following row's `begin_date` (or the station's last data month, or the trailing MSHR record's `end_date`). No gaps or overlaps exist between rows.

**Per-row metadata**:
- **lat/lon**: From the MSHR record active at that row's start date (i.e., the most recent MSHR record with `begin_date ≤ row_begin`). Falls back to the station inventory if no MSHR record exists or the record has no coordinates.
- **elevation**: Ground elevation (feet) from the active MSHR record's `ELEV_GROUND` field. Falls back to the inventory elevation converted from metres to feet.
- **relocation flag**: The `distance_and_direction` field (11 characters) is populated from the MSHR `RELOCATION` field (first 11 characters) when a row begins at a MSHR `begin_date` and that record's `RELOCATION` field is non-blank. This signals to TOBMain that the station moved at this date. Rows that do not correspond to a MSHR `begin_date`, or whose MSHR record has a blank `RELOCATION` field, leave `distance_and_direction` blank.

### Code Selection within Phase 2 (Tie-Breaking)

When multiple codes produce perfect fit, choose using priority order:

1. **Fewest distinct values**: 1 better than 2 better than 3
2. **Extends current run**: Prefer code matching previous segment (longer runs, fewer transitions)
3. **Common codes**: Prefer standard observation times (07HR, 17HR, 08HR, 18HR, 24HR)
4. **Alphabetically first**: Deterministic fallback

**Rationale**: All perfect fits are mathematically equivalent. We favor codes that produce simpler histories (fewer transitions) and avoid unusual codes unless necessary.

### Walk-Back for TOB+PHA Cases (within Phase 2)

When TOB change and PHA step occur within 18 months, forward scan may not find perfect fit until after both transitions. Walk backwards from the stable zone to determine transition order.

**Scenario**:
- Previous segment ended at month T1
- No perfect fit from T1+1 to T1+18
- Perfect fit resumes at T1+18+
- Need to determine: TOB first then PHA, or PHA first then TOB?

**Method**:
- Start from where perfect fit resumed
- Walk backwards up to 12 months
- Find where perfect fit first appears in the gap
- This reveals which transition occurred first

**Distinguishing TOB vs PHA**:
- **TOB transition**: Residual pattern changes (different monthly structure)
- **PHA step**: Residual pattern stays same, but offset shifts

Check by grouping residuals by calendar month:
- If same calendar month has varying residuals across years → TOB change
- If same calendar month has constant residuals (just shifted offset) → PHA step

**Output**:
- Create two segments filling the gap
- TOB change segment: `include_in_his=True`
- PHA step segment: `include_in_his=False`

### Gap Handling (within Phase 2)

Gaps ≥12 months in data coverage force explicit boundary creation:

**Rationale**:
- Gap likely indicates station move, instrumentation change, or observing practice change
- Missing months don't contribute to residual sets, so codes could remain "valid" across gaps
- Forcing boundaries ensures each segment has continuous data coverage

**Implementation**: During forward scan, check distance from last data month. If ≥12 months, end current segment and start new one.

## Example: Forward Scan in Action

**Station ABC, months 1-21**:

**Month 1-12** (initial window):
- Code 07HR: residuals {5, 6} cents → 2 distinct ✓
- Code 08HR: residuals {10, 11, 12} cents → 3 distinct ✓
- Code 17HR: residuals {-5, -4, 5} cents → 3 distinct ✓
- Code 24HR: residuals {15, 16, 20, 25} cents → 4 distinct ✗
- Valid codes: [07HR, 08HR, 17HR]

**Month 13**:
- Code 07HR: {5, 6} → still 2 distinct ✓
- Code 08HR: {10, 11, 12, 15} → now 4 distinct ✗
- Code 17HR: {-5, -4, 5} → still 3 distinct ✓
- Valid codes: [07HR, 17HR]

**Months 14-20**: Both 07HR and 17HR remain valid

**Month 21**:
- Code 07HR: {5, 6, 94, 95} → 4 distinct ✗
- Code 17HR: {-5, -4, 5, 88} → 4 distinct ✗
- Valid codes: [] → **BOUNDARY DETECTED**

**Action**:
- Create segment 1-20 with code 07HR (chosen from last valid set)
- Start new segment at month 21
- Reset candidates to all codes
- Continue forward scan

## Algorithm Features

### Conceptual Simplicity

Boundaries emerge naturally where perfect fit breaks. No pre-computed splits or complex heuristics.

### Direct Perfect Fit Optimization

Always seeks ≤3 distinct values - the definition of perfect fit. No intermediate metrics.

### Minimal Parameters

Single core criterion: ≤3 distinct values for perfect fit. Few tunable parameters reduces overfitting risk.

### Natural Edge Case Handling

- **Sparse data**: Forward scan continues until codes differentiate
- **Multiple transitions**: Walk-back handles TOB+PHA within 18 months
- **Unusual codes**: Used only when necessary for perfect fit
- **Boundary errors**: Multi-level refinement achieves precise timing (month ±6, day within spike months)
- **Pathological patterns**: Pattern matching detects and fixes

### Proper PHA Handling

- Detects both TOB and PHA boundaries
- Distinguishes between transition types
- Excludes PHA-only changes from output
- Marks gaps between same-code pathological regions as PHA-only
- Prevents seeding PHA algorithm with false TOB transitions

### Transparency

At every step we can inspect:
- Which codes remain valid
- How many distinct values each has
- Why boundaries were created
- How boundaries were refined
- Why segments were merged

## Design Principles

| Principle | Implementation |
|-----------|----------------|
| **Core approach** | Grow segments until fit breaks |
| **Boundary detection** | Direct (perfect fit breaks when >3 distinct values) |
| **Parameters** | Single criterion (≤3 distinct values) |
| **PHA handling** | Walk-back distinguishes types; timing validation detects PHA-only; gap marking for same-code regions |
| **Pathological patterns** | Pattern matching (±0.01°C) grows regions; stops at PHA boundaries |
| **Short bridge recoding** | Recode if neighbour code gives strictly fewer distinct values; strict-tie means current code is genuinely best |
| **Tie-breaking** | Fewest values → longest runs → common codes |
| **Gap handling** | Explicit detection (≥12 months → force boundary) |
| **TOB+PHA cases** | Walk-back during forward scan |
| **Boundary precision** | Backtrack refinement + timing validation (±6 months) + day-level for spikes |
| **Computational cost** | O(T × K) integrated scan where T=months, K=codes |
| **Modularity** | Single forward scan with refinement phases |

**Key strengths**:
- Simpler conceptual model
- Direct optimization for perfect fit
- Minimal tunable parameters
- Conservative boundary creation
- Precise boundary timing through multi-level refinement
- Robust PHA handling at multiple stages

## Implementation Notes

### Integer Math for Distinct Values

Always use integer arithmetic (cents) to avoid floating-point comparison errors. Rounding residuals to 0.01°C means working with integer cents.

**Wrong**: `distinct = len(set(residuals))` where residuals are floats (0.14 - 0.07 = 0.070000000001)

**Right**: `distinct = len(set(residual_cents))` where each residual is rounded to integer cents

### Efficient Set Tracking

Maintain a set of observed values for each candidate code. Update incrementally as scanning forward - no need to recompute entire window each month.

### Validation Metric

A segment passes validation if:
1. Perfect fit criterion: ≤3 distinct values in residuals
2. Low variance: variance < 0.05°C² (std dev < 0.22°C)

### Common Code Preference

Common observation times: 07HR, 17HR, 08HR, 18HR, 24HR, 06HR, 16HR

Unusual codes (21HR, 23HR, etc.) indicate complex transitions or mixed regimes - only used when necessary for perfect fit.

## Performance

On the full USHCN dataset (27,955 stations):
- 9,471 stations required TOB reconstruction
- ~99.8% validation success rate expected
- Average processing time: <1 second per station

The algorithm achieves high success by:
1. Growing segments until perfect fit breaks
2. Refining boundaries to exact shift points
3. Validating boundary timing (±6 months adjustment)
4. Refining spike boundaries (day-level for out-of-bound transitions)
5. Detecting and fixing pathological segmentation
6. Recoding short bridge segments when a neighbour's code fits better
7. Distinguishing TOB from PHA transitions
8. Marking PHA-only gaps between same-code regions
9. Merging same-code segments

## Summary

This algorithm represents a clean approach to TOB reconstruction:
- Boundaries emerge from data (fit breaks) rather than heuristics
- Direct optimization for perfect fit (≤3 distinct values)
- Minimal parameters to tune
- Transparent and debuggable
- Multi-level boundary refinement for precision
- Comprehensive PHA handling prevents output contamination

**Key insight**: Let the data tell us where boundaries are by growing segments until they can't grow anymore, rather than pre-computing splits and then fitting.
