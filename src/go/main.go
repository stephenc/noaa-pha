package main

import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"math"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
)

type YearData struct {
	Values [12]int
	QC     [12]byte
}

type InventoryEntry struct {
	Lat  float64
	Lon  float64
	Name string
}

type SumCount struct {
	Sum   float64
	Count int
}

type StationInfo struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	HasRef      bool   `json:"has_ref"`
	Breakpoints *int   `json:"breakpoints"`
}

type StationRow struct {
	Year   int        `json:"year"`
	Values []*float64 `json:"values"`
	Diffs  []*float64 `json:"diffs"`
}

type HisEntry struct {
	Year      float64 `json:"year"`
	TobChange bool    `json:"tob_change"`
	TobCode   string  `json:"tob_code"`
}

type ViewerApp struct {
	leftDir    string
	rightDir   string
	right2Dir  string
	historyDir string
	leftMap    map[string]string
	rightMap   map[string]string
	right2Map  map[string]string
	historyMap map[string]string
	inv        map[string]InventoryEntry
	leftHash    string
	rightHash   string
	right2Hash  string
	invHash     string
	historyHash string

	cacheMu sync.Mutex
	cache   map[string]any

	seriesMu    sync.Mutex
	seriesCache map[string]any

	template string
}

type Handler struct {
	app *ViewerApp
}

const (
	latGridStep         = 2.5
	lonGridStep         = 3.75
	globalLatGridCells  = int(180.0 / latGridStep)
	globalLonGridCells  = int(360.0 / lonGridStep)
	totalGlobalGridCell = globalLatGridCells * globalLonGridCells
	monthsPerYear       = 12.0
)

func listStationFiles(dir string) map[string]string {
	mapping := make(map[string]string)
	entries, err := os.ReadDir(dir)
	if err != nil {
		return mapping
	}
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		stationID := name
		if idx := strings.Index(name, "."); idx >= 0 {
			stationID = name[:idx]
		}
		if stationID != "" {
			mapping[stationID] = filepath.Join(dir, name)
		}
	}
	return mapping
}

func hashDirState(dir string) string {
	if dir == "" {
		return ""
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		return ""
	}
	names := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		names = append(names, entry.Name())
	}
	sort.Strings(names)
	h := sha256.New()
	for _, name := range names {
		info, err := os.Stat(filepath.Join(dir, name))
		if err != nil {
			continue
		}
		h.Write([]byte(name))
		h.Write([]byte(fmt.Sprintf("%d", info.Size())))
		h.Write([]byte(fmt.Sprintf("%d", info.ModTime().Unix())))
	}
	return hex.EncodeToString(h.Sum(nil))
}

func hashFileState(path string) string {
	if path == "" {
		return ""
	}
	info, err := os.Stat(path)
	if err != nil {
		return ""
	}
	h := sha256.New()
	h.Write([]byte(filepath.Base(path)))
	h.Write([]byte(fmt.Sprintf("%d", info.Size())))
	h.Write([]byte(fmt.Sprintf("%d", info.ModTime().Unix())))
	return hex.EncodeToString(h.Sum(nil))
}

func parseStationFile(path string) (map[int]YearData, error) {
	data := make(map[int]YearData)
	fh, err := os.Open(path)
	if err != nil {
		return data, err
	}
	defer fh.Close()

	reader := bufio.NewReader(fh)
	for {
		line, err := reader.ReadBytes('\n')
		if len(line) > 0 {
			if len(line) < 16+12*9 {
				if err == io.EOF {
					break
				}
				if err != nil {
					return data, err
				}
				continue
			}
			yearStr := strings.TrimSpace(string(line[12:16]))
			year, yerr := strconv.Atoi(yearStr)
			if yerr != nil {
				if err == io.EOF {
					break
				}
				if err != nil {
					return data, err
				}
				continue
			}
			var yd YearData
			for month := 0; month < 12; month++ {
				offset := 16 + month*9
				valueRaw := strings.TrimSpace(string(line[offset : offset+6]))
				value := -9999
				if valueRaw != "" {
					if v, verr := strconv.Atoi(valueRaw); verr == nil {
						value = v
					}
				}
				qc := byte(0)
				flag := line[offset+6 : offset+9]
				if len(flag) >= 2 {
					qc = flag[1]
				}
				yd.Values[month] = value
				yd.QC[month] = qc
			}
			data[year] = yd
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return data, err
		}
	}
	return data, nil
}

func parseInventory(path string) map[string]InventoryEntry {
	inv := make(map[string]InventoryEntry)
	if path == "" {
		return inv
	}
	fh, err := os.Open(path)
	if err != nil {
		return inv
	}
	defer fh.Close()

	reader := bufio.NewReader(fh)
	for {
		line, err := reader.ReadBytes('\n')
		if len(line) > 0 {
			if len(line) < 30 {
				if err == io.EOF {
					break
				}
				if err != nil {
					return inv
				}
				continue
			}
			stationID := string(line[0:11])
			latStr := strings.TrimSpace(string(line[12:20]))
			lonStr := strings.TrimSpace(string(line[21:30]))
			lat, latErr := strconv.ParseFloat(latStr, 64)
			lon, lonErr := strconv.ParseFloat(lonStr, 64)
			if latErr != nil || lonErr != nil {
				if err == io.EOF {
					break
				}
				if err != nil {
					return inv
				}
				continue
			}
			name := ""
			if len(line) >= 68 {
				name = strings.TrimSpace(string(line[38:68]))
			}
			inv[stationID] = InventoryEntry{Lat: lat, Lon: lon, Name: name}
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return inv
		}
	}
	return inv
}

func parseHisFile(path string) []HisEntry {
	fh, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer fh.Close()

	type rawRow struct {
		year, month, day int
		tobCode          string
	}
	var rows []rawRow

	scanner := bufio.NewScanner(fh)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 || line[0] != '2' {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) < 3 {
			continue
		}
		// Determine which field holds the begin-date (8 digits).
		// Our generated format has no station-ID token, so fields[1] is the
		// date.  Some original NOAA files include a station-ID token so the
		// date may be at fields[2].
		dateIdx := 1
		if len(fields[1]) != 8 {
			dateIdx = 2
		}
		if dateIdx >= len(fields) || len(fields[dateIdx]) != 8 {
			continue
		}
		ds := fields[dateIdx]
		year, err1 := strconv.Atoi(ds[0:4])
		month, err2 := strconv.Atoi(ds[4:6])
		day, err3 := strconv.Atoi(ds[6:8])
		if err1 != nil || err2 != nil || err3 != nil {
			continue
		}
		tobCode := fields[len(fields)-1]
		rows = append(rows, rawRow{year, month, day, tobCode})
	}
	if scanner.Err() != nil || len(rows) < 2 {
		return nil
	}

	yearFracFor := func(r rawRow) float64 {
		t := time.Date(r.year, time.Month(r.month), r.day, 0, 0, 0, 0, time.UTC)
		yStart := time.Date(r.year, 1, 1, 0, 0, 0, 0, time.UTC)
		yEnd := time.Date(r.year+1, 1, 1, 0, 0, 0, 0, time.UTC)
		return float64(r.year) + t.Sub(yStart).Hours()/yEnd.Sub(yStart).Hours()
	}
	entries := make([]HisEntry, 0, len(rows))
	// Always include the first row so the initial TOB code label is shown.
	entries = append(entries, HisEntry{
		Year:      yearFracFor(rows[0]),
		TobChange: true,
		TobCode:   rows[0].tobCode,
	})
	for i := 1; i < len(rows); i++ {
		entries = append(entries, HisEntry{
			Year:      yearFracFor(rows[i]),
			TobChange: rows[i].tobCode != rows[i-1].tobCode,
			TobCode:   rows[i].tobCode,
		})
	}
	return entries
}

func gridID(lat float64, lon float64) int {
	if lon < 0 {
		lon += 360.0
	}
	latIndex := int(math.Floor((lat - 0.0) / latGridStep))
	lonIndex := int(math.Floor((lon - 0.0) / lonGridStep))
	return latIndex*globalLonGridCells + lonIndex + 1
}

func shouldInclude(qc byte, includeQC bool) bool {
	if includeQC {
		return true
	}
	return qc == ' '
}

func floatPtr(v float64) *float64 {
	return &v
}

func computeOverallMean(leftMap map[string]string, includeQC bool) (float64, int) {
	total := 0.0
	count := 0
	for _, path := range leftMap {
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for _, yd := range data {
			for i := 0; i < 12; i++ {
				v := yd.Values[i]
				qc := yd.QC[i]
				if v == -9999 {
					continue
				}
				if !shouldInclude(qc, includeQC) {
					continue
				}
				total += float64(v) / 100.0
				count++
			}
		}
	}
	return total, count
}

func computeOverallDiff(leftMap map[string]string, rightMap map[string]string, includeQC bool) (float64, int) {
	total := 0.0
	count := 0
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		years := make([]int, 0, len(leftData))
		for year := range leftData {
			if _, ok := rightData[year]; ok {
				years = append(years, year)
			}
		}
		for _, year := range years {
			lvals := leftData[year]
			rvals := rightData[year]
			for i := 0; i < 12; i++ {
				lv := lvals.Values[i]
				rv := rvals.Values[i]
				if lv == -9999 || rv == -9999 {
					continue
				}
				if !shouldInclude(lvals.QC[i], includeQC) || !shouldInclude(rvals.QC[i], includeQC) {
					continue
				}
				total += (float64(lv) - float64(rv)) / 100.0
				count++
			}
		}
	}
	return total, count
}

func computeGridMean(leftMap map[string]string, inv map[string]InventoryEntry, includeQC bool) map[int]SumCount {
	grid := make(map[int]SumCount)
	for stationID, path := range leftMap {
		entry, ok := inv[stationID]
		if !ok {
			continue
		}
		gid := gridID(entry.Lat, entry.Lon)
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for _, yd := range data {
			for i := 0; i < 12; i++ {
				v := yd.Values[i]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[i], includeQC) {
					continue
				}
				sc := grid[gid]
				sc.Sum += float64(v) / 100.0
				sc.Count++
				grid[gid] = sc
			}
		}
	}
	return grid
}

func computeGridDiff(leftMap map[string]string, rightMap map[string]string, inv map[string]InventoryEntry, includeQC bool) map[int]SumCount {
	grid := make(map[int]SumCount)
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		entry, ok := inv[stationID]
		if !ok {
			continue
		}
		gid := gridID(entry.Lat, entry.Lon)
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		for year, lvals := range leftData {
			rvals, ok := rightData[year]
			if !ok {
				continue
			}
			for i := 0; i < 12; i++ {
				lv := lvals.Values[i]
				rv := rvals.Values[i]
				if lv == -9999 || rv == -9999 {
					continue
				}
				if !shouldInclude(lvals.QC[i], includeQC) || !shouldInclude(rvals.QC[i], includeQC) {
					continue
				}
				sc := grid[gid]
				sc.Sum += (float64(lv) - float64(rv)) / 100.0
				sc.Count++
				grid[gid] = sc
			}
		}
	}
	return grid
}

func timeKey(year int, month int, granularity string) string {
	if granularity == "yearly" || granularity == "count" {
		return fmt.Sprintf("%04d", year)
	}
	return fmt.Sprintf("%04d-%02d", year, month)
}

func normalizeGranularity(granularity string) string {
	switch granularity {
	case "monthly", "yearly", "count":
		return granularity
	default:
		return "monthly"
	}
}

type Point struct {
	T string  `json:"t"`
	V float64 `json:"v"`
}

func finalizeSeries(sumCount map[string]SumCount) []Point {
	keys := make([]string, 0, len(sumCount))
	for k := range sumCount {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	points := make([]Point, 0, len(keys))
	for _, k := range keys {
		sc := sumCount[k]
		if sc.Count == 0 {
			continue
		}
		points = append(points, Point{T: k, V: sc.Sum / float64(sc.Count)})
	}
	return points
}

func finalizeValueSeries(values map[string]float64) []Point {
	keys := make([]string, 0, len(values))
	for k := range values {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	points := make([]Point, 0, len(keys))
	for _, k := range keys {
		points = append(points, Point{T: k, V: values[k]})
	}
	return points
}

func normalizeCountsPerMonth(rawCounts map[string]float64) map[string]float64 {
	normalized := make(map[string]float64, len(rawCounts))
	for key, count := range rawCounts {
		normalized[key] = count / monthsPerYear
	}
	return normalized
}

func diffCountSeries(leftCounts map[string]float64, rightCounts map[string]float64) []Point {
	diffCounts := make(map[string]float64, len(leftCounts)+len(rightCounts))
	for key, v := range leftCounts {
		diffCounts[key] += v
	}
	for key, v := range rightCounts {
		diffCounts[key] -= v
	}
	return finalizeValueSeries(diffCounts)
}

func computeOverallSeriesCount(leftMap map[string]string, includeQC bool) []Point {
	counts := make(map[string]float64)
	for _, path := range leftMap {
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for year, yd := range data {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				counts[key]++
			}
		}
	}
	return finalizeValueSeries(normalizeCountsPerMonth(counts))
}

func computeOverallSeriesCompareCount(leftMap map[string]string, rightMap map[string]string, includeQC bool) ([]Point, []Point, []Point) {
	leftCounts := make(map[string]float64)
	rightCounts := make(map[string]float64)
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		for year, yd := range leftData {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				leftCounts[key]++
			}
		}
		for year, yd := range rightData {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				rightCounts[key]++
			}
		}
	}
	leftNorm := normalizeCountsPerMonth(leftCounts)
	rightNorm := normalizeCountsPerMonth(rightCounts)
	return finalizeValueSeries(leftNorm), finalizeValueSeries(rightNorm), diffCountSeries(leftNorm, rightNorm)
}

func normalizeGridCountSeries(rawCounts map[string]float64) []Point {
	normalized := make(map[string]float64, len(rawCounts))
	denom := monthsPerYear * float64(totalGlobalGridCell)
	if denom <= 0 {
		return finalizeValueSeries(normalized)
	}
	for key, count := range rawCounts {
		normalized[key] = count / denom
	}
	return finalizeValueSeries(normalized)
}

func computeGridSeriesCount(leftMap map[string]string, inv map[string]InventoryEntry, includeQC bool) []Point {
	counts := make(map[string]float64)
	for stationID, path := range leftMap {
		if _, ok := inv[stationID]; !ok {
			continue
		}
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for year, yd := range data {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				counts[key]++
			}
		}
	}
	return normalizeGridCountSeries(counts)
}

func computeGridSeriesCompareCount(leftMap map[string]string, rightMap map[string]string, inv map[string]InventoryEntry, includeQC bool) ([]Point, []Point, []Point) {
	leftRawCounts := make(map[string]float64)
	rightRawCounts := make(map[string]float64)
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		if _, ok := inv[stationID]; !ok {
			continue
		}
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		for year, yd := range leftData {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				leftRawCounts[key]++
			}
		}
		for year, yd := range rightData {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, "count")
				rightRawCounts[key]++
			}
		}
	}
	leftValues := make(map[string]float64, len(leftRawCounts))
	rightValues := make(map[string]float64, len(rightRawCounts))
	denom := monthsPerYear * float64(totalGlobalGridCell)
	if denom > 0 {
		for key, count := range leftRawCounts {
			leftValues[key] = count / denom
		}
		for key, count := range rightRawCounts {
			rightValues[key] = count / denom
		}
	}
	return finalizeValueSeries(leftValues), finalizeValueSeries(rightValues), diffCountSeries(leftValues, rightValues)
}

func computeOverallSeries(leftMap map[string]string, includeQC bool, granularity string) []Point {
	sums := make(map[string]SumCount)
	for _, path := range leftMap {
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for year, yd := range data {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, granularity)
				sc := sums[key]
				sc.Sum += float64(v) / 100.0
				sc.Count++
				sums[key] = sc
			}
		}
	}
	return finalizeSeries(sums)
}

func computeOverallSeriesCompare(leftMap map[string]string, rightMap map[string]string, includeQC bool, granularity string) ([]Point, []Point, []Point) {
	leftSums := make(map[string]SumCount)
	rightSums := make(map[string]SumCount)
	diffSums := make(map[string]SumCount)
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		for year, lvals := range leftData {
			rvals, ok := rightData[year]
			if !ok {
				continue
			}
			for idx := 0; idx < 12; idx++ {
				lv := lvals.Values[idx]
				rv := rvals.Values[idx]
				if lv == -9999 || rv == -9999 {
					continue
				}
				if !shouldInclude(lvals.QC[idx], includeQC) || !shouldInclude(rvals.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, granularity)
				ls := leftSums[key]
				rs := rightSums[key]
				ds := diffSums[key]
				ls.Sum += float64(lv) / 100.0
				ls.Count++
				rs.Sum += float64(rv) / 100.0
				rs.Count++
				ds.Sum += (float64(lv) - float64(rv)) / 100.0
				ds.Count++
				leftSums[key] = ls
				rightSums[key] = rs
				diffSums[key] = ds
			}
		}
	}
	return finalizeSeries(leftSums), finalizeSeries(rightSums), finalizeSeries(diffSums)
}

func computeGridSeries(leftMap map[string]string, inv map[string]InventoryEntry, includeQC bool, granularity string) []Point {
	buckets := make(map[string]map[int]SumCount)
	for stationID, path := range leftMap {
		entry, ok := inv[stationID]
		if !ok {
			continue
		}
		gid := gridID(entry.Lat, entry.Lon)
		data, err := parseStationFile(path)
		if err != nil {
			continue
		}
		for year, yd := range data {
			for idx := 0; idx < 12; idx++ {
				v := yd.Values[idx]
				if v == -9999 {
					continue
				}
				if !shouldInclude(yd.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, granularity)
				bucket := buckets[key]
				if bucket == nil {
					bucket = make(map[int]SumCount)
					buckets[key] = bucket
				}
				sc := bucket[gid]
				sc.Sum += float64(v) / 100.0
				sc.Count++
				bucket[gid] = sc
			}
		}
	}
	series := make(map[string]SumCount)
	for key, grids := range buckets {
		gridMeans := make([]float64, 0, len(grids))
		for _, sc := range grids {
			if sc.Count > 0 {
				gridMeans = append(gridMeans, sc.Sum/float64(sc.Count))
			}
		}
		if len(gridMeans) == 0 {
			continue
		}
		sum := 0.0
		for _, m := range gridMeans {
			sum += m
		}
		series[key] = SumCount{Sum: sum, Count: len(gridMeans)}
	}
	return finalizeSeries(series)
}

func computeGridSeriesCompare(leftMap map[string]string, rightMap map[string]string, inv map[string]InventoryEntry, includeQC bool, granularity string) ([]Point, []Point, []Point) {
	bucketsLeft := make(map[string]map[int]SumCount)
	bucketsRight := make(map[string]map[int]SumCount)
	bucketsDiff := make(map[string]map[int]SumCount)
	for stationID, leftPath := range leftMap {
		rightPath, ok := rightMap[stationID]
		if !ok {
			continue
		}
		entry, ok := inv[stationID]
		if !ok {
			continue
		}
		gid := gridID(entry.Lat, entry.Lon)
		leftData, err := parseStationFile(leftPath)
		if err != nil {
			continue
		}
		rightData, err := parseStationFile(rightPath)
		if err != nil {
			continue
		}
		for year, lvals := range leftData {
			rvals, ok := rightData[year]
			if !ok {
				continue
			}
			for idx := 0; idx < 12; idx++ {
				lv := lvals.Values[idx]
				rv := rvals.Values[idx]
				if lv == -9999 || rv == -9999 {
					continue
				}
				if !shouldInclude(lvals.QC[idx], includeQC) || !shouldInclude(rvals.QC[idx], includeQC) {
					continue
				}
				key := timeKey(year, idx+1, granularity)
				bl := bucketsLeft[key]
				if bl == nil {
					bl = make(map[int]SumCount)
					bucketsLeft[key] = bl
				}
				br := bucketsRight[key]
				if br == nil {
					br = make(map[int]SumCount)
					bucketsRight[key] = br
				}
				bd := bucketsDiff[key]
				if bd == nil {
					bd = make(map[int]SumCount)
					bucketsDiff[key] = bd
				}
				lsc := bl[gid]
				rsc := br[gid]
				dsc := bd[gid]
				lsc.Sum += float64(lv) / 100.0
				lsc.Count++
				rsc.Sum += float64(rv) / 100.0
				rsc.Count++
				dsc.Sum += (float64(lv) - float64(rv)) / 100.0
				dsc.Count++
				bl[gid] = lsc
				br[gid] = rsc
				bd[gid] = dsc
			}
		}
	}
	finalize := func(buckets map[string]map[int]SumCount) []Point {
		series := make(map[string]SumCount)
		for key, grids := range buckets {
			gridMeans := make([]float64, 0, len(grids))
			for _, sc := range grids {
				if sc.Count > 0 {
					gridMeans = append(gridMeans, sc.Sum/float64(sc.Count))
				}
			}
			if len(gridMeans) == 0 {
				continue
			}
			sum := 0.0
			for _, m := range gridMeans {
				sum += m
			}
			series[key] = SumCount{Sum: sum, Count: len(gridMeans)}
		}
		return finalizeSeries(series)
	}
	return finalize(bucketsLeft), finalize(bucketsRight), finalize(bucketsDiff)
}

func subsetStationMap(source map[string]string, stationIDs []string) map[string]string {
	subset := make(map[string]string, len(stationIDs))
	for _, sid := range stationIDs {
		id := strings.TrimSpace(sid)
		if id == "" {
			continue
		}
		if path, ok := source[id]; ok {
			subset[id] = path
		}
	}
	return subset
}

func (app *ViewerApp) computeStationSeriesCount(includeQC bool, stationIDs []string, compare bool, refNum int) map[string]any {
	leftCounts := make(map[string]float64)
	rightCounts := make(map[string]float64)
	hasRefAny := false

	refMap := app.rightMap
	if refNum == 2 {
		refMap = app.right2Map
	}

	for _, sid := range stationIDs {
		id := strings.TrimSpace(sid)
		if id == "" {
			continue
		}
		leftPath, ok := app.leftMap[id]
		if !ok {
			continue
		}
		leftData, err := parseStationFile(leftPath)
		if err == nil {
			for year, yd := range leftData {
				for idx := 0; idx < 12; idx++ {
					v := yd.Values[idx]
					if v == -9999 {
						continue
					}
					if !shouldInclude(yd.QC[idx], includeQC) {
						continue
					}
					key := timeKey(year, idx+1, "count")
					leftCounts[key]++
				}
			}
		}

		if compare {
			rightPath, ok := refMap[id]
			if !ok {
				continue
			}
			rightData, err := parseStationFile(rightPath)
			if err != nil {
				continue
			}
			hasRefAny = true
			for year, yd := range rightData {
				for idx := 0; idx < 12; idx++ {
					v := yd.Values[idx]
					if v == -9999 {
						continue
					}
					if !shouldInclude(yd.QC[idx], includeQC) {
						continue
					}
					key := timeKey(year, idx+1, "count")
					rightCounts[key]++
				}
			}
		}
	}

	if len(leftCounts) == 0 {
		return map[string]any{"error": "no station data"}
	}

	leftNorm := normalizeCountsPerMonth(leftCounts)
	if compare && hasRefAny {
		rightNorm := normalizeCountsPerMonth(rightCounts)
		series := []any{
			map[string]any{"label": "left", "points": finalizeValueSeries(leftNorm)},
			map[string]any{"label": "right", "points": finalizeValueSeries(rightNorm)},
		}
		return map[string]any{"series": series, "diff": diffCountSeries(leftNorm, rightNorm)}
	}
	return map[string]any{"series": []any{map[string]any{"label": "left", "points": finalizeValueSeries(leftNorm)}}}
}

func countBreakpoints(leftPath string, rightPath string, includeQC bool) int {
	leftData, err := parseStationFile(leftPath)
	if err != nil {
		return 0
	}
	rightData, err := parseStationFile(rightPath)
	if err != nil {
		return 0
	}

	// Collect all differences
	years := make([]int, 0, len(leftData))
	for year := range leftData {
		if _, ok := rightData[year]; ok {
			years = append(years, year)
		}
	}
	sort.Ints(years)

	diffs := make([]int, 0)
	for _, year := range years {
		lvals := leftData[year]
		rvals := rightData[year]
		for i := 0; i < 12; i++ {
			lv := lvals.Values[i]
			rv := rvals.Values[i]
			if lv == -9999 || rv == -9999 {
				continue
			}
			if !shouldInclude(lvals.QC[i], includeQC) || !shouldInclude(rvals.QC[i], includeQC) {
				continue
			}
			diffs = append(diffs, lv-rv)
		}
	}

	if len(diffs) == 0 {
		return 0
	}

	// Helper to check if a set of values can be described as (x-1, x, x+1) pattern
	// This allows for ±0.01°C variation (±1 in integer units of 0.01°C)
	canBeTightCluster := func(values map[int]bool) bool {
		if len(values) <= 3 {
			// Find min and max
			minVal := int(1e9)
			maxVal := int(-1e9)
			for v := range values {
				if v < minVal {
					minVal = v
				}
				if v > maxVal {
					maxVal = v
				}
			}
			// Check if all values are within ±1 of the center
			center := (minVal + maxVal) / 2
			for v := range values {
				if v < center-1 || v > center+1 {
					return false
				}
			}
			return true
		}
		return false
	}

	// Track segments of consistent residuals
	count := 0
	currentSegmentValues := make(map[int]bool)
	currentSegmentValues[diffs[0]] = true

	for i := 1; i < len(diffs); i++ {
		diff := diffs[i]

		// Try adding this value to the current segment
		testValues := make(map[int]bool)
		for k := range currentSegmentValues {
			testValues[k] = true
		}
		testValues[diff] = true

		// Check if current segment is still valid (≤3 unique values or tight cluster)
		if len(testValues) <= 3 && canBeTightCluster(testValues) {
			// Continue current segment
			currentSegmentValues[diff] = true
		} else {
			// Start new segment - this is a breakpoint
			count++
			currentSegmentValues = make(map[int]bool)
			currentSegmentValues[diff] = true
		}
	}

	return count
}

func NewViewerApp(leftDir, rightDir, right2Dir, historyDir, inventory, templatePath string) (*ViewerApp, error) {
	app := &ViewerApp{
		leftDir:     leftDir,
		rightDir:    rightDir,
		right2Dir:   right2Dir,
		historyDir:  historyDir,
		leftMap:     listStationFiles(leftDir),
		rightMap:    map[string]string{},
		right2Map:   map[string]string{},
		historyMap:  map[string]string{},
		inv:         map[string]InventoryEntry{},
		cache:       make(map[string]any),
		seriesCache: make(map[string]any),
	}
	if rightDir != "" {
		app.rightMap = listStationFiles(rightDir)
	}
	if right2Dir != "" {
		app.right2Map = listStationFiles(right2Dir)
	}
	if historyDir != "" {
		app.historyMap = listStationFiles(historyDir)
	}
	if inventory != "" {
		app.inv = parseInventory(inventory)
	}
	app.leftHash = hashDirState(leftDir)
	app.rightHash = hashDirState(rightDir)
	app.right2Hash = hashDirState(right2Dir)
	app.historyHash = hashDirState(historyDir)
	app.invHash = hashFileState(inventory)

	tmpl, err := os.ReadFile(templatePath)
	if err != nil {
		return nil, err
	}
	app.template = string(tmpl)

	go app.precomputeSeries()
	return app, nil
}

func (app *ViewerApp) getHisData(stationID string) []HisEntry {
	app.cacheMu.Lock()
	defer app.cacheMu.Unlock()
	key := "his:" + stationID
	if cached, ok := app.cache[key]; ok {
		if v, ok2 := cached.([]HisEntry); ok2 {
			return v
		}
	}
	path, ok := app.historyMap[stationID]
	if !ok {
		return nil
	}
	entries := parseHisFile(path)
	app.cache[key] = entries
	return entries
}

func (app *ViewerApp) seriesCacheKey(mode string, includeQC bool, granularity string, compare bool) string {
	return fmt.Sprintf("%s:%t:%s:%t", mode, includeQC, granularity, compare)
}

func (app *ViewerApp) precomputeSeries() {
	total := 0
	done := 0
	compareFlags := []bool{false}
	if app.rightDir != "" || app.right2Dir != "" {
		compareFlags = []bool{true}
	}
	refNums := make([]int, 0, 2)
	if app.rightDir != "" {
		refNums = append(refNums, 1)
	}
	if app.right2Dir != "" {
		refNums = append(refNums, 2)
	}
	if len(refNums) == 0 {
		refNums = append(refNums, 1)
	}

	type task struct {
		mode        string
		includeQC   bool
		granularity string
		compare     bool
		refNum      int
	}
	tasks := make([]task, 0, 16)
	for _, includeQC := range []bool{false, true} {
		for _, granularity := range []string{"yearly", "monthly", "count"} {
			for _, mode := range []string{"overall", "grid"} {
				if mode == "grid" && len(app.inv) == 0 {
					continue
				}
				for _, compare := range compareFlags {
					taskRefNums := []int{1}
					if compare {
						taskRefNums = refNums
					}
					for _, refNum := range taskRefNums {
						tasks = append(tasks, task{
							mode:        mode,
							includeQC:   includeQC,
							granularity: granularity,
							compare:     compare,
							refNum:      refNum,
						})
						total++
					}
				}
			}
		}
	}

	workerCount := runtime.NumCPU()
	if workerCount < 1 {
		workerCount = 1
	}
	taskCh := make(chan task)
	var wg sync.WaitGroup

	worker := func() {
		defer wg.Done()
		for t := range taskCh {
			key := app.seriesCacheKey(t.mode, t.includeQC, t.granularity, t.compare)
			cacheKey := fmt.Sprintf("%s:ref%d", key, t.refNum)
			app.seriesMu.Lock()
			_, ok := app.seriesCache[cacheKey]
			app.seriesMu.Unlock()
			if ok {
				continue
			}
			result := app.computeSeriesUncached(t.mode, t.includeQC, t.granularity, t.compare, t.refNum)
			app.seriesMu.Lock()
			app.seriesCache[cacheKey] = result
			done++
			current := done
			app.seriesMu.Unlock()
			log.Printf("[viewer] precompute %d/%d ready: %s", current, total, cacheKey)
		}
	}

	wg.Add(workerCount)
	for i := 0; i < workerCount; i++ {
		go worker()
	}
	for _, t := range tasks {
		taskCh <- t
	}
	close(taskCh)
	wg.Wait()
}

func (app *ViewerApp) computeSeriesWithMaps(mode string, includeQC bool, granularity string, compare bool, refNum int, leftMap map[string]string) map[string]any {
	granularity = normalizeGranularity(granularity)

	// Select which ref map to use
	refMap := app.rightMap
	if refNum == 2 {
		refMap = app.right2Map
	}

	if mode == "overall" {
		if granularity == "count" {
			if compare {
				left, right, diff := computeOverallSeriesCompareCount(leftMap, refMap, includeQC)
				return map[string]any{"series": []any{
					map[string]any{"label": "left", "points": left},
					map[string]any{"label": "right", "points": right},
				}, "diff": diff}
			}
			return map[string]any{"series": []any{map[string]any{"label": "count", "points": computeOverallSeriesCount(leftMap, includeQC)}}}
		}
		if compare {
			left, right, diff := computeOverallSeriesCompare(leftMap, refMap, includeQC, granularity)
			return map[string]any{"series": []any{
				map[string]any{"label": "left", "points": left},
				map[string]any{"label": "right", "points": right},
			}, "diff": diff}
		}
		return map[string]any{"series": []any{map[string]any{"label": "mean", "points": computeOverallSeries(leftMap, includeQC, granularity)}}}
	}
	if mode == "grid" {
		if len(app.inv) == 0 {
			return map[string]any{"error": "inventory required"}
		}
		if granularity == "count" {
			if compare {
				left, right, diff := computeGridSeriesCompareCount(leftMap, refMap, app.inv, includeQC)
				return map[string]any{"series": []any{
					map[string]any{"label": "left", "points": left},
					map[string]any{"label": "right", "points": right},
				}, "diff": diff}
			}
			return map[string]any{"series": []any{map[string]any{"label": "grid-count", "points": computeGridSeriesCount(leftMap, app.inv, includeQC)}}}
		}
		if compare {
			left, right, diff := computeGridSeriesCompare(leftMap, refMap, app.inv, includeQC, granularity)
			return map[string]any{"series": []any{
				map[string]any{"label": "left", "points": left},
				map[string]any{"label": "right", "points": right},
			}, "diff": diff}
		}
		return map[string]any{"series": []any{map[string]any{"label": "grid-mean", "points": computeGridSeries(leftMap, app.inv, includeQC, granularity)}}}
	}
	return map[string]any{"error": "unknown mode"}
}

func (app *ViewerApp) computeSeriesUncached(mode string, includeQC bool, granularity string, compare bool, refNum int) map[string]any {
	return app.computeSeriesWithMaps(mode, includeQC, granularity, compare, refNum, app.leftMap)
}

func (app *ViewerApp) getStationList(includeQC bool) []StationInfo {
	key := fmt.Sprintf("stations:%t", includeQC)
	app.cacheMu.Lock()
	if cached, ok := app.cache[key]; ok {
		app.cacheMu.Unlock()
		return cached.([]StationInfo)
	}
	app.cacheMu.Unlock()

	stationIDs := make([]string, 0, len(app.leftMap))
	for id := range app.leftMap {
		stationIDs = append(stationIDs, id)
	}
	sort.Strings(stationIDs)
	stations := make([]StationInfo, 0, len(stationIDs))
	for _, stationID := range stationIDs {
		_, hasRef := app.rightMap[stationID]
		var bps *int
		if hasRef {
			val := countBreakpoints(app.leftMap[stationID], app.rightMap[stationID], includeQC)
			bps = &val
		}
		name := ""
		if entry, ok := app.inv[stationID]; ok {
			name = entry.Name
		}
		stations = append(stations, StationInfo{ID: stationID, Name: name, HasRef: hasRef, Breakpoints: bps})
	}

	app.cacheMu.Lock()
	app.cache[key] = stations
	app.cacheMu.Unlock()
	return stations
}

func (app *ViewerApp) summary(mode string, includeQC bool) map[string]any {
	key := fmt.Sprintf("summary:%s:%t", mode, includeQC)
	app.cacheMu.Lock()
	if cached, ok := app.cache[key]; ok {
		app.cacheMu.Unlock()
		return cached.(map[string]any)
	}
	app.cacheMu.Unlock()

	var result map[string]any
	switch mode {
	case "overall":
		total, count := computeOverallMean(app.leftMap, includeQC)
		var mean any
		if count > 0 {
			mean = total / float64(count)
		} else {
			mean = nil
		}
		result = map[string]any{"mean": mean, "count": count}
	case "overall_ref":
		total, count := computeOverallDiff(app.leftMap, app.rightMap, includeQC)
		var mean any
		if count > 0 {
			mean = total / float64(count)
		} else {
			mean = nil
		}
		result = map[string]any{"mean": mean, "count": count}
	case "grid":
		if len(app.inv) == 0 {
			result = map[string]any{"error": "inventory required"}
		} else {
			grid := computeGridMean(app.leftMap, app.inv, includeQC)
			gridOut := make(map[string]any)
			for gid, sc := range grid {
				var mean any
				if sc.Count > 0 {
					mean = sc.Sum / float64(sc.Count)
				} else {
					mean = nil
				}
				gridOut[strconv.Itoa(gid)] = map[string]any{"mean": mean, "count": sc.Count}
			}
			result = map[string]any{"grid": gridOut}
		}
	case "grid_ref":
		if len(app.inv) == 0 {
			result = map[string]any{"error": "inventory required"}
		} else {
			grid := computeGridDiff(app.leftMap, app.rightMap, app.inv, includeQC)
			gridOut := make(map[string]any)
			for gid, sc := range grid {
				var mean any
				if sc.Count > 0 {
					mean = sc.Sum / float64(sc.Count)
				} else {
					mean = nil
				}
				gridOut[strconv.Itoa(gid)] = map[string]any{"mean": mean, "count": sc.Count}
			}
			result = map[string]any{"grid": gridOut}
		}
	default:
		result = map[string]any{"error": "unknown mode"}
	}

	app.cacheMu.Lock()
	app.cache[key] = result
	app.cacheMu.Unlock()
	return result
}

func (app *ViewerApp) stationData(stationID string, includeQC bool, compare bool) map[string]any {
	leftPath, ok := app.leftMap[stationID]
	if !ok {
		return map[string]any{"error": "station not found"}
	}
	leftData, err := parseStationFile(leftPath)
	if err != nil {
		return map[string]any{"error": "station not found"}
	}
	var rightData map[int]YearData
	if compare {
		if rightPath, ok := app.rightMap[stationID]; ok {
			if rd, err := parseStationFile(rightPath); err == nil {
				rightData = rd
			}
		}
	}

	years := make([]int, 0, len(leftData))
	for year := range leftData {
		years = append(years, year)
	}
	sort.Ints(years)

	rows := make([]StationRow, 0, len(years))
	for _, year := range years {
		lvals := leftData[year]
		row := StationRow{Year: year, Values: make([]*float64, 0, 12), Diffs: make([]*float64, 0, 12)}
		var rvals *YearData
		if rightData != nil {
			if rv, ok := rightData[year]; ok {
				rvals = &rv
			}
		}
		for m := 0; m < 12; m++ {
			lv := lvals.Values[m]
			lq := lvals.QC[m]
			if lv == -9999 || !shouldInclude(lq, includeQC) {
				row.Values = append(row.Values, nil)
			} else {
				v := float64(lv) / 100.0
				row.Values = append(row.Values, floatPtr(v))
			}
			if rvals != nil {
				rv := rvals.Values[m]
				rq := rvals.QC[m]
				if rv == -9999 || !shouldInclude(rq, includeQC) || lv == -9999 || !shouldInclude(lq, includeQC) {
					row.Diffs = append(row.Diffs, nil)
				} else {
					v := (float64(lv) - float64(rv)) / 100.0
					row.Diffs = append(row.Diffs, floatPtr(v))
				}
			}
		}
		rows = append(rows, row)
	}
	return map[string]any{"rows": rows, "has_ref": rightData != nil}
}

func (app *ViewerApp) series(mode string, includeQC bool, granularity string, stationID string, compare bool, refNum int) map[string]any {
	granularity = normalizeGranularity(granularity)

	if mode == "overall" || mode == "grid" {
		if stationID != "" {
			stationIDs := strings.Split(stationID, ",")
			leftSubset := subsetStationMap(app.leftMap, stationIDs)
			if len(leftSubset) == 0 {
				return map[string]any{"error": "no station data"}
			}
			return app.computeSeriesWithMaps(mode, includeQC, granularity, compare, refNum, leftSubset)
		}
		key := app.seriesCacheKey(mode, includeQC, granularity, compare) + fmt.Sprintf(":ref%d", refNum)
		app.seriesMu.Lock()
		cached, ok := app.seriesCache[key]
		app.seriesMu.Unlock()
		if ok {
			return cached.(map[string]any)
		}
		result := app.computeSeriesUncached(mode, includeQC, granularity, compare, refNum)
		app.seriesMu.Lock()
		app.seriesCache[key] = result
		app.seriesMu.Unlock()
		return result
	}
	if mode == "station" {
		if stationID == "" {
			return map[string]any{"error": "station id required"}
		}
		stationIDs := strings.Split(stationID, ",")
		if granularity == "count" {
			return app.computeStationSeriesCount(includeQC, stationIDs, compare, refNum)
		}
		leftSums := make(map[string]SumCount)
		rightSums := make(map[string]SumCount)
		diffSums := make(map[string]SumCount)
		hasRefAny := false

		// Select which ref map to use
		refMap := app.rightMap
		if refNum == 2 {
			refMap = app.right2Map
		}

		for _, sid := range stationIDs {
			if sid == "" {
				continue
			}
			leftPath, ok := app.leftMap[sid]
			if !ok {
				continue
			}
			leftData, err := parseStationFile(leftPath)
			if err != nil {
				continue
			}
			var rightData map[int]YearData
			if compare {
				if rightPath, ok := refMap[sid]; ok {
					if rd, err := parseStationFile(rightPath); err == nil {
						rightData = rd
						hasRefAny = true
					}
				}
			}
			for year, lvals := range leftData {
				var rvals *YearData
				if rightData != nil {
					if rv, ok := rightData[year]; ok {
						rvals = &rv
					}
				}
				for idx := 0; idx < 12; idx++ {
					lv := lvals.Values[idx]
					lq := lvals.QC[idx]
					if lv == -9999 || !shouldInclude(lq, includeQC) {
						continue
					}
					key := timeKey(year, idx+1, granularity)
					ls := leftSums[key]
					ls.Sum += float64(lv) / 100.0
					ls.Count++
					leftSums[key] = ls
					if compare && rvals != nil {
						rv := rvals.Values[idx]
						rq := rvals.QC[idx]
						if rv == -9999 || !shouldInclude(rq, includeQC) {
							continue
						}
						rs := rightSums[key]
						ds := diffSums[key]
						rs.Sum += float64(rv) / 100.0
						rs.Count++
						ds.Sum += (float64(lv) - float64(rv)) / 100.0
						ds.Count++
						rightSums[key] = rs
						diffSums[key] = ds
					}
				}
			}
		}
		if len(leftSums) == 0 {
			return map[string]any{"error": "no station data"}
		}
		leftPoints := finalizeSeries(leftSums)
		series := []any{map[string]any{"label": "left", "points": leftPoints}}
		if compare && hasRefAny {
			rightPoints := finalizeSeries(rightSums)
			diffPoints := finalizeSeries(diffSums)
			series = append(series, map[string]any{"label": "right", "points": rightPoints})
			return map[string]any{"series": series, "diff": diffPoints}
		}
		return map[string]any{"series": series}
	}
	return map[string]any{"error": "unknown mode"}
}

func (h *Handler) etagFor(kind string, params any) string {
	payload, _ := json.Marshal(params)
	parts := []string{kind, h.app.leftHash, h.app.rightHash, h.app.right2Hash, h.app.invHash, string(payload)}
	sum := sha256.Sum256([]byte(strings.Join(parts, "::")))
	return fmt.Sprintf("W/\"%x\"", sum)
}

func (h *Handler) maybe304(w http.ResponseWriter, r *http.Request, etag string) bool {
	if r.Header.Get("If-None-Match") == etag {
		w.Header().Set("ETag", etag)
		w.WriteHeader(http.StatusNotModified)
		return true
	}
	return false
}

func (h *Handler) sendJSON(w http.ResponseWriter, r *http.Request, payload any, etag string) {
	body, err := json.Marshal(payload)
	if err != nil {
		http.Error(w, "failed to encode response", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Content-Length", strconv.Itoa(len(body)))
	w.Header().Set("ETag", etag)
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write(body)
}

func (h *Handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		w.WriteHeader(http.StatusMethodNotAllowed)
		return
	}
	switch r.URL.Path {
	case "/":
		hasRef := "false"
		hasRef2 := "false"
		hasHis := "false"
		if h.app.rightDir != "" {
			hasRef = "true"
		}
		if h.app.right2Dir != "" {
			hasRef2 = "true"
		}
		if h.app.historyDir != "" {
			hasHis = "true"
		}
		body := strings.ReplaceAll(h.app.template, "__HAS_REF__", hasRef)
		body = strings.ReplaceAll(body, "__HAS_REF2__", hasRef2)
		body = strings.ReplaceAll(body, "__HAS_HIS__", hasHis)
		body = strings.ReplaceAll(body, "__LEFT_LABEL__", htmlEscape(h.app.leftDir))
		body = strings.ReplaceAll(body, "__RIGHT_LABEL__", htmlEscape(h.app.rightDir))
		body = strings.ReplaceAll(body, "__RIGHT2_LABEL__", htmlEscape(h.app.right2Dir))
		w.Header().Set("Content-Type", "text/html")
		w.Header().Set("Content-Length", strconv.Itoa(len(body)))
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte(body))
		return
	case "/api/stations":
		includeQC := r.URL.Query().Get("include_qc") == "1"
		params := struct {
			IncludeQC bool `json:"include_qc"`
		}{IncludeQC: includeQC}
		etag := h.etagFor("stations", params)
		if h.maybe304(w, r, etag) {
			return
		}
		stations := h.app.getStationList(includeQC)
		h.sendJSON(w, r, stations, etag)
		return
	case "/api/summary":
		includeQC := r.URL.Query().Get("include_qc") == "1"
		mode := r.URL.Query().Get("mode")
		if mode == "" {
			mode = "overall"
		}
		params := struct {
			IncludeQC bool   `json:"include_qc"`
			Mode      string `json:"mode"`
		}{IncludeQC: includeQC, Mode: mode}
		etag := h.etagFor("summary", params)
		if h.maybe304(w, r, etag) {
			return
		}
		h.sendJSON(w, r, h.app.summary(mode, includeQC), etag)
		return
	case "/api/series":
		includeQC := r.URL.Query().Get("include_qc") == "1"
		mode := r.URL.Query().Get("mode")
		if mode == "" {
			mode = "overall"
		}
		compare := r.URL.Query().Get("compare") == "1"
		granularity := r.URL.Query().Get("granularity")
		if granularity == "" {
			granularity = "monthly"
		}
		granularity = normalizeGranularity(granularity)
		stationID := r.URL.Query().Get("station_id")
		refNum := 1 // Default to ref1
		if r.URL.Query().Get("ref") == "2" {
			refNum = 2
		}
		params := struct {
			IncludeQC   bool   `json:"include_qc"`
			Mode        string `json:"mode"`
			Compare     bool   `json:"compare"`
			Granularity string `json:"granularity"`
			StationID   string `json:"station_id"`
			RefNum      int    `json:"ref_num"`
		}{IncludeQC: includeQC, Mode: mode, Compare: compare, Granularity: granularity, StationID: stationID, RefNum: refNum}
		etag := h.etagFor("series", params)
		if h.maybe304(w, r, etag) {
			return
		}
		h.sendJSON(w, r, h.app.series(mode, includeQC, granularity, stationID, compare, refNum), etag)
		return
	case "/api/his":
		stationID := r.URL.Query().Get("station_id")
		if stationID == "" || h.app.historyDir == "" {
			h.sendJSON(w, r, []HisEntry{}, "")
			return
		}
		params := struct {
			StationID   string `json:"station_id"`
			HistoryHash string `json:"history_hash"`
		}{StationID: stationID, HistoryHash: h.app.historyHash}
		etag := h.etagFor("his", params)
		if h.maybe304(w, r, etag) {
			return
		}
		entries := h.app.getHisData(stationID)
		if entries == nil {
			entries = []HisEntry{}
		}
		h.sendJSON(w, r, entries, etag)
		return
	case "/api/station":
		stationID := r.URL.Query().Get("id")
		includeQC := r.URL.Query().Get("include_qc") == "1"
		compare := r.URL.Query().Get("compare") == "1"
		params := struct {
			IncludeQC bool   `json:"include_qc"`
			Compare   bool   `json:"compare"`
			StationID string `json:"station_id"`
		}{IncludeQC: includeQC, Compare: compare, StationID: stationID}
		etag := h.etagFor("station", params)
		if h.maybe304(w, r, etag) {
			return
		}
		h.sendJSON(w, r, h.app.stationData(stationID, includeQC, compare), etag)
		return
	default:
		w.WriteHeader(http.StatusNotFound)
		return
	}
}

func htmlEscape(input string) string {
	replacer := strings.NewReplacer(
		"&", "&amp;",
		"<", "&lt;",
		">", "&gt;",
		"\"", "&quot;",
		"'", "&#39;",
	)
	return replacer.Replace(input)
}

func main() {
	var (
		dir       string
		ref       string
		ref2      string
		history   string
		inventory string
		host      string
		port      int
	)
	flag.StringVar(&dir, "dir", "", "Primary data directory")
	flag.StringVar(&ref, "ref", "", "Reference data directory")
	flag.StringVar(&ref2, "ref2", "", "Second reference data directory")
	flag.StringVar(&history, "history", "", "History (.his) file directory")
	flag.StringVar(&inventory, "inventory", "", "Station inventory file")
	flag.StringVar(&host, "host", "127.0.0.1", "Host to bind")
	flag.IntVar(&port, "port", 8080, "Port to bind")
	flag.Parse()

	if strings.TrimSpace(dir) == "" {
		log.Fatal("--dir is required")
	}

	templatePath := filepath.Join(filepath.Dir(os.Args[0]), "static", "index.html")
	if _, err := os.Stat(templatePath); err != nil {
		templatePath = filepath.Join("src", "go", "static", "index.html")
	}

	app, err := NewViewerApp(dir, ref, ref2, history, inventory, templatePath)
	if err != nil {
		log.Fatalf("failed to start viewer: %v", err)
	}

	addr := fmt.Sprintf("%s:%d", host, port)
	server := &http.Server{
		Addr:              addr,
		Handler:           &Handler{app: app},
		ReadHeaderTimeout: 5 * time.Second,
	}

	log.Printf("Viewer running at http://%s", addr)
	if err := server.ListenAndServe(); err != nil {
		log.Fatal(err)
	}
}
