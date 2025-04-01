FROM ubuntu:latest

# Install GNU Fortran and necessary tools
RUN apt-get update && apt-get install -y \
    gfortran \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container
COPY . /app

# Default command
CMD ["gfortran", "--version"]
