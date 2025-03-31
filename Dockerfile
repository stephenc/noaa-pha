# Use a Debian stable base image with a slim variant for smaller size
FROM debian:bookworm-slim AS builder

# Set metadata labels
LABEL maintainer="Your Name <your.email@example.com>"
LABEL description="Build environment for the Fortran USHCN project"

# Set the working directory inside the container
WORKDIR /app

# Install necessary dependencies: gfortran, make, python3, and coreutils (for mkdir etc.)
# Combine update, install, and cleanup in one RUN layer to reduce image size
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        gfortran \
        make \
        python3 \
        coreutils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copy the source code, Makefile, and dependency script into the container
# Copy specific items instead of '.' to avoid including unwanted local files/dirs
COPY src ./src
COPY Makefile .
COPY generate_deps.py .

# Run make to build the project.
# This will generate deps.mk first, then compile and link everything.
# The executables will be in /app/bin inside the container.
RUN make

# --- Final Stage (Optional, if you want a smaller runtime image) ---
# If you only need the final binaries, you can create a minimal runtime image.
# If you want the full build environment (e.g., for debugging), skip this stage.

FROM debian:bookworm-slim AS runtime

WORKDIR /app

# Install runtime dependencies if any (e.g., specific libraries the executables need)
RUN apt-get update && apt-get install -y --no-install-recommends gawk && rm -rf /var/lib/apt/lists/*

ENV PATH=/app/bin:$PATH

# Copy only the built executables from the builder stage
COPY --from=builder /app/bin ./bin
COPY src/awk ./src/awk

CMD ["bash"]
