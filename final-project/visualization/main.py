import numpy as np
import matplotlib.pyplot as plt
import os
from moviepy.video.io.ImageSequenceClip import ImageSequenceClip
import re

def carregar_matriz_temperatura(caminho_arquivo):
    return np.loadtxt(caminho_arquivo, delimiter=",")

def plotar_heatmap(T, step, salvar=True, pasta_saida="dump/plot"):
    plt.figure(figsize=(6, 5))

    extent = [0, T.shape[1], 0, T.shape[0]]
    im = plt.imshow(T, cmap='hot', origin='lower', extent=extent, aspect='equal')

    plt.title(f"Distribuição de Temperatura - Step {step}")
    plt.xlabel("x (cm)")
    plt.ylabel("y (cm)")

    cbar = plt.colorbar(im)
    cbar.set_label("Temperatura (°C)")

    if salvar:
        nome_saida = os.path.join(pasta_saida, f"heatmap_step_{step:05d}.png")
        os.makedirs(os.path.dirname(nome_saida), exist_ok=True)
        plt.savefig(nome_saida, dpi=150, bbox_inches='tight')
    else:
        plt.show()

    plt.close()

def gerar_video(pasta_imagens, nome_video="video_heatmap.mp4", fps=10):
    arquivos = [
        os.path.join(pasta_imagens, f)
        for f in os.listdir(pasta_imagens)
        if f.endswith(".png")
    ]

    def extrair_step(nome_arquivo):
        base = os.path.basename(nome_arquivo)
        match = re.search(r"step_(\d+)", base)
        return int(match.group(1)) if match else -1

    arquivos_ordenados = sorted(arquivos, key=extrair_step)

    print(f"Gerando vídeo com {len(arquivos_ordenados)} frames em '{pasta_imagens}'...")

    clip = ImageSequenceClip(arquivos_ordenados, fps=fps)
    clip.write_videofile(nome_video, codec='libx264')
    print(f"Vídeo salvo como {nome_video}")

def processar_arquivos(prefixo, pasta_dump="dump", pasta_saida="dump/plot"):
    caminho_completo = os.path.join(os.getcwd(), pasta_dump)

    arquivos = sorted([
        f for f in os.listdir(caminho_completo)
        if f.startswith(prefixo) and f.endswith(".csv")
    ])

    for arquivo in arquivos:
        step = int(arquivo.split(prefixo)[1].split(".")[0])
        caminho_arquivo = os.path.join(caminho_completo, arquivo)
        T = carregar_matriz_temperatura(caminho_arquivo)
        plotar_heatmap(T, step, salvar=True, pasta_saida=pasta_saida)

def main():
    # Processa arquivos Julia
    processar_arquivos("jl_T_", pasta_dump="dump", pasta_saida="dump/plot_julia")
    gerar_video("dump/plot_julia", nome_video="heatmap_diffusao_julia.mp4", fps=10)

    # Processa arquivos Fortran
    processar_arquivos("f_T_", pasta_dump="dump", pasta_saida="dump/plot_fortran")
    gerar_video("dump/plot_fortran", nome_video="heatmap_diffusao_fortran.mp4", fps=10)

if __name__ == "__main__":
    main()
