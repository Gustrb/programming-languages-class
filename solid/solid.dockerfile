FROM ubuntu:latest

# Instalar Ruby e ferramentas necessárias
RUN apt-get update && apt-get install -y \
    ruby-full \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Definir o diretório de trabalho
WORKDIR /app

# Copiar o conteúdo atual para dentro do container
COPY . /app

# Abrir o terminal
CMD ["bash"]
