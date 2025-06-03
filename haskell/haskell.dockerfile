FROM ubuntu:latest

# Instala GHC e ferramentas necessárias
RUN apt-get update && apt-get install -y \
    ghc \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Define o diretório de trabalho
WORKDIR /app

# Copia o conteúdo atual para dentro do container
COPY . /app

# Abre o terminal ao iniciar o container
CMD ["bash"]
