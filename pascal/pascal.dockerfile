FROM ubuntu:latest

# Install Free Pascal and necessary tools
RUN apt-get update && apt-get install -y \
    fpc \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container
COPY . /app

# open the terminal
CMD ["bash"]