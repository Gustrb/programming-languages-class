FROM debian:latest

RUN apt-get update && \
    apt-get install -y gcc make gdb valgrind && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

CMD ["bash"]
