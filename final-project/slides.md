---
title: "Comparativo: Julia vs Fortran em Aplicações Científicas"
author: "Grupo G2: Gustavo Bauer"
date: "08/07/2025"
---

# Capa
- **DISCIPLINA:** Linguagens de Programação  
- **PROFESSOR:** Leonardo Sápiras  
- **CURSO:** Sistemas de Informação  
- **TRABALHO G2 :** Aplicações Científicas  

---

# Descrição da Tarefa
- Domínio escolhido: **Aplicações Científicas**  
- Comparar 2 linguagens: **Julia** e **Fortran**  
- Critérios:
  - Apresentação do domínio e suas características  
  - Como cada linguagem se destaca  
  - Prós e contras  
  - Exemplos (serão mostrados ao vivo)  
  - Tempo total: 9 min; participação de todos  

---

# Por que “Aplicações Científicas”?
- Prototipação rápida + alta performance  
- Física, engenharia, bioinformática, meteorologia…  
- Cálculo numérico intensivo  

---

# Caso de Estudo: Difusão de Calor
- Cozimento de um peru em forno  
- PDE:

$$
\frac{\partial u}{\partial t}
= \alpha\Bigl(\frac{\partial^2u}{\partial x^2}
+ \frac{\partial^2u}{\partial y^2}\Bigr)
$$  

$$\alpha \approx 1.4\times10^{-7}\,\mathrm{m^2/s}$$

---

# Intuição Física e Método Numérico
- Calor flui de quente → frio  
- Se vizinhos estão mais quentes, $u$ sobe; se mais frios, $u$ cai  
- Diferenças finitas explícitas:

$$
r = \frac{\alpha\,\Delta t}{\Delta x^2},\quad r\le0.25
\Rightarrow
T_{i,j}^{n+1}
= T_{i,j}^n
+ r\,(T_{i\pm1,j}^n + T_{i,j\pm1}^n - 4\,T_{i,j}^n)
$$

---

# Julia – Pontos Fortes
- REPL interativo, sintaxe de alto nível  
- Pacotes: DifferentialEquations.jl, Plots.jl…  
- JIT → performance “quase C”  
- I/O, paralelismo e GC automáticos  

---

# Fortran – Pontos Fortes
- Linguagem tradicional de HPC  
- Tipagem estática, controle de memória  
- OpenMP/MPI ready  
- Overhead quase zero em loops numéricos  

---

# Demonstração ao Vivo
- Rodando um passo de tempo em Julia e Fortran  
- Checando \(r\), gerando dump & plot  

---

# Prós e Contras
**Julia**  
+ Produtividade e sintaxe limpa  
– Warm‐up JIT, GC imprevisível em HPC  

**Fortran**  
+ Velocidade máxima, maturidade em HPC  
– Sintaxe verbosa, menos “baterias incluídas”  

---

# Conclusões
- Julia: ótima para protótipos rápidos  
- Fortran: primeira escolha em produção HPC  
- **Julia** para exploratório, **Fortran** para produção  

---

# Perguntas
- julialang.org  
- ISO Fortran Standard  
- Códigos no Classroom  
*Obrigado!*