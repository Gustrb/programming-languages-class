using DelimitedFiles  # para salvar arquivos .csv

using Printf


function main()
    nx, ny = 50, 50
    dx = 0.0025  # 0.25 cm em metros
    dt = 20.0 / 5000  # mesma duração total de 20 s
    alpha = 1.4e-7

    total_time = 20.0  # segundos simulados
    steps = 500000

    r = alpha * dt / dx^2
    println("r = $r")

    @assert r ≤ 0.25 "r = $r instável! Ajuste dt ou dx."

    T = fill(5.0, nx, ny)
    T[:, 1] .= 180.0;
    T[:, end] .= 180.0
    T[1, :] .= 180.0;
    T[end, :] .= 180.0

    T_new = similar(T)

    for step = 1:steps
        T[:, 1] .= 180.0;
        T[:, end] .= 180.0
        T[1, :] .= 180.0;
        T[end, :] .= 180.0

        for i = 2:(nx-1), j = 2:(ny-1)
            T_new[i, j] =
                T[i, j] + r * (T[i+1, j] + T[i-1, j] + T[i, j+1] + T[i, j-1] - 4*T[i, j])
        end

        T_new[:, 1] .= 180.0;
        T_new[:, end] .= 180.0
        T_new[1, :] .= 180.0;
        T_new[end, :] .= 180.0

        copy!(T, T_new)

        # Salva a cada 1000 steps
        if step % 4000 == 0
            filename = @sprintf("visualization/dump/jl_T_%09d.csv", step)
            writedlm(filename, T, ',')
        end
    end
end

main()
