# run_model_nest.jl
# Nested production function case
# The difference from run_model.jl are highlighed in the comments.

function model_inner_nest(W; N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ_inner, α_inner, ρ_outer, α_outer, θ, ind_us, P_start)
    # ρ_outer, α_outer, ρ_inner, α_inner: intermediate input production function
    # α_outer = weights on value-added

    NX = nx.*W[ind_us]

    γ = (gamma.((θ .+ 1 .- η)./θ)).^(1 ./ (1 .- η))

    # Contraction mapping to derive R_outer and P_sec
    function PR_contract(P_sec_s)
        R_inner = Array{Float64}(undef, N, S)
        R_outer = Array{Float64}(undef, N, S)
        for i in 1:N
            for k in 1:S
                # Price for the composite intermediate
                R_inner[i,k] = sum(α_inner[i,1:S,k].*(P_sec_s[i,:].^(1-ρ_inner[k])))^(1/(1-ρ_inner[k]))
                # Unit price (α_outer denotes VA share parameter.)
                R_outer[i,k] = ((1-α_outer[i,k])*R_inner[i,k]^(1-ρ_outer[k]) + α_outer[i,k]*W[i]^(1-ρ_outer[k]))^(1/(1-ρ_outer[k]))
            end
        end

        P_sec = Array{Float64}(undef, N,S)
        for j in 1:N
            for k in 1:S
                P_sec[j,k] = γ[k]*(sum((R_outer[:,k].*τ[:,j,k]./ Ā[:,k]).^(-θ[k])))^(-1/θ[k])
            end
        end

        return(P_sec, R_outer, R_inner)
    end

    # This is where it takes most of the time.
    P_sec_s = P_start
    # P_sec_s = 1e-8.*ones(N,S)
    iter = 0
    dist = 1
    tol_p_r = 1e-13

    while dist > tol_p_r
        (P_sec, R_outer, R_inner) = PR_contract(P_sec_s)
        dist = max(abs.(P_sec_s - P_sec)...)
        P_sec_s = P_sec
        iter += 1
    end

    (P_sec, R_outer, R_inner) = PR_contract(P_sec_s)

    # Derive C_f and P_f by solving two equations simulatenously.
    function CP(c_i, p_i, NX_pc_i, w_i, ϕ_i) 
        return(((w_i-NX_pc_i)/c_i - sum(ϕ_i.*(p_i.^(1-σ)).*c_i.^((1-σ).*(ϵ.-1)))^(1/(1-σ)))^2)
    end

    if (prod(ϵ .== 1)) & (σ != 1)
        P_f = sum(ϕ.*P_sec.^(1-σ), dims = 2).^(1/(1-σ))
        C_f = (W.-(NX./L))./P_f
    elseif (prod(ϵ .== 1)) & (σ == 1)
        P_f = prod(P_sec.^ϕ, dims = 2)./prod(ϕ.^ϕ, dims = 2)
        C_f = (W.-(NX./L))./P_f
    else
        C_f = Array{Float64}(undef, N)
        for i in 1:N
            temp = optimize(c_i -> CP(c_i, P_sec[i,:], NX[i]/L[i], W[i], ϕ[i,:]), 0.0, 1e7)
            if !(Optim.converged(temp))
                error("C_f optimization not converged")
            end
            C_f[i] = temp.minimizer
        end
        P_f = (W.-(NX./L))./C_f
    end

    # Using C_f, P_f, and P_sec, derive C_sec
    C_sec = Array{Float64}(undef, N, S)

    for i in 1:N
        C_sec[i,:] = ϕ[i,:].*((P_sec[i,:]./P_f[i]).^(-σ)).*(C_f[i].^((1-σ).*(ϵ.-1).+1))
    end

    # Derive trade share π (here I use "sh" for share so that I don't get confused with 3.1415)
    gr = Array{Float64}(undef, N,N,S)

    for i in 1:N
        for j in 1:N
            for k in 1:S
                gr[i,j,k] = (1/Ā[i,k]*R_outer[i,k]*τ[i,j,k])^(-θ[k])
            end
        end
    end

    sh = Array{Float64}(undef, N,N,S)

    for i in 1:N
        for j in 1:N
            for k in 1:S
                sh[i,j,k] = gr[i,j,k]/sum(gr[:,j,k])
            end
        end
    end

    ψ = Array{Float64}(undef, N, S, S)

    for i in 1:N
        for k in 1:S
            for h in 1:S
                # share of k among total production costs for country-i-sector-h
                # (Share of k among intermediate composite) * (Share of composite among total input costs)
                ψ[i,k,h] = α_inner[i,k,h]*(P_sec[i,k]^(1-ρ_inner[h]))*(R_inner[i,h]^(ρ_inner[h]-1))*
                           ((1-α_outer[i,h]))*R_inner[i,h]^(1-ρ_outer[h])*(R_outer[i,h]^(ρ_outer[h]-1))
            end
        end
    end

    # How much of composite (i,h) flow into (j,k) as intermediate inputs?
    ψ_share_mat = Array{Float64}(undef, N*S, N*S)
    for h in 1:S # origin sector
        for k in 1:S # destination sector
            for i in 1:N # origin country
                for j in 1:N # destination country
                    ψ_share_mat[(h-1)*N + i, (k-1)*N + j] = ψ[i,h,k]*sh[i,j,k]
                end
            end
        end
    end

    PQ_sec_vec = inv(Diagonal(ones(N*S, N*S)) .- ψ_share_mat)*(vcat((L.*P_sec.*C_sec)...))

    PQ_sec = reshape(PQ_sec_vec, N,S)

    Q_sec = PQ_sec./P_sec

    M_sec = Array{Float64}(undef, N, S, S)
    for i in 1:N
        for k in 1:S
            for h in 1:S
                M_sec[i,k,h] = ψ[i,k,h]*sum(sh[i,:,h].*P_sec[:,h].*Q_sec[:,h])/P_sec[i,k]
            end
        end
    end

    # Derive excess demand function
    Z = Array{Float64}(undef, N)

    for i in 1:N
        Z[i] = 1/W[i]*(sum(sh[i,:,:].*P_sec.*Q_sec) - NX[i] - sum(P_sec[i,:].*Q_sec[i,:]))
    end

    # Derive labor allocations
    L_sec = Array{Float64}(undef, N, S)
    for i in 1:N
        for k in 1:S
            L_sec[i,k] = α_outer[i,k]*W[i]^(1-ρ_outer[k])/(R_outer[i,k]^(1-ρ_outer[k]))*sum(sh[i,:,k].*P_sec[:,k].*Q_sec[:,k])/W[i]
        end
    end

    return(DataFrame(Z = [Z], P_f = [P_f], C_f = [C_f], P_sec = [P_sec], R_inner = [R_inner], R_outer = [R_outer], Q_sec = [Q_sec], C_sec = [C_sec], M_sec = [M_sec], L_sec = [L_sec], sh = [sh]))
end

# Solve the model for a single year
function model_solve_nest(;W_init, κ, max_it, N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ_inner, ρ_outer, α_inner, α_outer, θ, ind_us, P_start)
    if !(sum(W_init.*L) ≈ 1)
        error("the starting point is not in Δ_w")
    end

    inner(W) = model_inner_nest(W; N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ_inner, ρ_outer, α_inner, α_outer, θ, ind_us, P_start)

    dist = 1
    iter = 0
    tol_w = 1e-9 # The accuracy you want. (1e-22) (1e-20) 7 base Another room for performance gain?
    W_start = W_init
    while dist > tol_w
        W_after = W_start.*(1.0 .+ κ.*((inner(W_start)).Z[1])./L)
        dist = max(abs.(W_after.-W_start)...)
        W_start = W_after
        iter += 1
        if !(sum(W_start.*L) ≈ 1)
            println(sum(W_start.*L))
            error("the starting point is not in Δ_w")
        end
        if iter == max_it
            error("reached maximum number of iterations")
        end
    end

    res = inner(W_start)

    if max((abs.(sum(res.L_sec[1], dims = 2).-L)./L)...) > 1e-2 #1e-7
        error("labor market clearing condition is not met: max(abs(model allocations - data allocations)/(data allocations)) are ", max((abs.(sum(res.L_sec[1], dims = 2).-L)./L)...))
    end

    res.W = [W_start]

    return(res)
end

function run_model_nest(;seq_years, W_init_seq, κ = 0.1, max_it = 1e5, N, S, L_seq, Ā_seq, τ_seq, nx_seq, σ, ϵ, ϕ_seq, η, ρ_inner, ρ_outer, α_inner_seq, α_outer_seq, θ, ind_us, P_start_seq)
    res_years = DataFrame()
    for i_year in 1:length(seq_years)
        res_years = [res_years; model_solve_nest(;W_init = W_init_seq[i_year], κ = κ, max_it = max_it, N, S, L = L_seq[i_year], Ā = Ā_seq[i_year], τ = τ_seq[i_year], nx = nx_seq[i_year], σ, ϵ, ϕ = ϕ_seq[i_year], η, ρ_inner, ρ_outer, α_inner = α_inner_seq[i_year], α_outer = α_outer_seq[i_year], θ, ind_us, P_start = P_start_seq[i_year])]
    end
    res_years.year = seq_years
    return(res_years)
end
