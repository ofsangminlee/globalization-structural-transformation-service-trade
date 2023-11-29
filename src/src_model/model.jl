# model.jl
# Contains functions to solve the equilibrium given productivities and trade costs across years (also labor and nx)
# Note: for autarky equilibria, any W (wage vector) satisfies equilibrium, because there won't be any global relative prices.

# model_inner: given model parameters and primitives, derive excess demand for a given wage vector (excess demand = 0 means its an equilibrium.)
function model_inner(W; N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ, α, θ, ind_us, P_start)
    # W: wage vector
    # N, S: Number of countries and sectors
    # L, Ā, τ, nx: labor endowment, productivities, trade costs, NX term
    # σ, ϵ, ϕ: utility function params
    # η: final goods production function params
    # ρ, α: intermediate input production function
    # θ: Frechet shape param
    # p_start: starting point for contraction for P and R

    # Note that NX terms are in terms of US labor, adjust them
    NX = nx.*W[ind_us]

    γ = (gamma.((θ .+ 1 .- η)./θ)).^(1 ./ (1 .- η)) # gamma function for price derivation

    # Contraction mapping to derive R and P_sec
    function PR_contract(P_sec_s)
        # Unit price of input
        R = Array{Float64}(undef, N, S)
        for i in 1:N
            for k in 1:S
                R[i,k] = (sum(α[i,1:S,k].*(P_sec_s[i,:].^(1-ρ[k]))) + α[i,S+1,k]*W[i]^(1-ρ[k]))^(1/(1-ρ[k]))
            end
        end

        P_sec = Array{Float64}(undef, N,S)
        for j in 1:N
            for k in 1:S
                P_sec[j,k] = γ[k]*(sum((R[:,k].*τ[:,j,k]./ Ā[:,k]).^(-θ[k])))^(-1/θ[k])
            end
        end

        return(P_sec, R)
    end

    # This is where it takes most of the time.
    P_sec_s = P_start
    # P_sec_s = 1e-8.*ones(N,S)
    iter = 0
    dist = 1
    tol_p_r = 1e-13 # (1e-20 original) 11

    while dist > tol_p_r
        (P_sec, R) = PR_contract(P_sec_s)
        dist = max(abs.(P_sec_s - P_sec)...)
        P_sec_s = P_sec
        iter += 1
    end

    (P_sec, R) = PR_contract(P_sec_s)

    # Derive C_f and P_f by solving two equations simulatenously.
    # Homothetic case, it's easier
    function CP(c_i, p_i, NX_pc_i, w_i, ϕ_i) # NX_pc_i: NX per capita for i
        return(((w_i-NX_pc_i)/c_i - sum(ϕ_i.*(p_i.^(1-σ)).*c_i.^((1-σ).*(ϵ.-1)))^(1/(1-σ)))^2)
    end

    if (prod(ϵ .== 1)) & (σ != 1)
        P_f = sum(ϕ.*P_sec.^(1-σ), dims = 2).^(1/(1-σ))
        C_f = (W.-(NX./L))./P_f
    elseif (prod(ϵ .== 1)) & (σ == 1) # I need this because of floating point. 0.999999^Inf = 0 vs 1^Inf = 1 ALSO ϕ should sum to one!!!! In this case.
        P_f = prod(P_sec.^ϕ, dims = 2)./prod(ϕ.^ϕ, dims = 2)
        C_f = (W.-(NX./L))./P_f
    else
        C_f = Array{Float64}(undef, N)
        for i in 1:N
            temp = optimize(c_i -> CP(c_i, P_sec[i,:], NX[i]/L[i], W[i], ϕ[i,:]), 0.0, 1e7) #1e10 (Note: here upper bound for optimization needs to be increased when you do experiments that increases productivities a lot.)
            if !(Optim.converged(temp))
                error("C_f optimization not converged")
            end
            C_f[i] = temp.minimizer
        end
        P_f = (W.-(NX./L))./C_f
        # println(min((W.-(NX./L))...))
    end

    # Using C_f, P_f, and P_sec, derive C_sec
    C_sec = Array{Float64}(undef, N, S)

    for i in 1:N
        C_sec[i,:] = ϕ[i,:].*((P_sec[i,:]./P_f[i]).^(-σ)).*(C_f[i].^((1-σ).*(ϵ.-1).+1))
    end

    # Derive trade share π (here I use "sh" for share so that I don't get confused with 3.1415)

    gr = Array{Float64}(undef, N,N,S) # the common term in the gravity equation

    for i in 1:N
        for j in 1:N
            for k in 1:S
                gr[i,j,k] = (1/Ā[i,k]*R[i,k]*τ[i,j,k])^(-θ[k])
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

    # Derive Q_sec by solving a system of linear equations from
    # 1. Q_sec = L C_sec + M_sec
    # 2. Demand function for M_sec given prices and wages

    ψ = Array{Float64}(undef, N, S, S)

    for i in 1:N
        for k in 1:S
            for h in 1:S
                # share of k among total production costs
                ψ[i,k,h] = α[i,k,h]*(P_sec[i,k]^(1-ρ[h]))*(R[i,h]^(ρ[h]-1))
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
            L_sec[i,k] = α[i,S+1,k]*W[i]^(1-ρ[k])/(R[i,k]^(1-ρ[k]))*sum(sh[i,:,k].*P_sec[:,k].*Q_sec[:,k])/W[i]
        end
    end
    # return((;Z = Z, P_f = P_f, C_f = C_f, P_sec = P_sec, R = R, Q_sec = Q_sec, C_sec = C_sec, M_sec = M_sec, L_sec = L_sec, sh = sh))
    return(DataFrame(Z = [Z], P_f = [P_f], C_f = [C_f], P_sec = [P_sec], R = [R], Q_sec = [Q_sec], C_sec = [C_sec], M_sec = [M_sec], L_sec = [L_sec], sh = [sh]))
end

# Solve the model for a single year
function model_solve(;W_init, κ, max_it, N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ, α, θ, ind_us, P_start)
    # Check whether the starting point is in Δ_w
    if !(sum(W_init.*L) ≈ 1)
        error("the starting point is not in Δ_w")
    end

    inner(W) = model_inner(W; N, S, L, Ā, τ, nx, σ, ϵ, ϕ, η, ρ, α, θ, ind_us, P_start)

    # Solve the model by iterating
    dist = 1
    iter = 0
    tol_w = 1e-9 # The accuracy you want. (1e-22) (1e-20) 7 base Another room for performance gain?
    W_start = W_init
    while dist > tol_w
        # println(iter)
        # println(min(W_start...))
        W_after = W_start.*(1.0 .+ κ.*((inner(W_start)).Z[1])./L)
        # println("Max of Z is ", max(inner(W_start)[1]...))
        dist = max(abs.(W_after.-W_start)...)
        # println("Distance between W_start and W_after is ", dist)
        W_start = W_after
        iter += 1
        # println(iter)
        # if !(sum(W_start.*L) ≈ scale_simplex)
        # I think this step will be unnecessary. Let's see.
        if !(sum(W_start.*L) ≈ 1)
            println(sum(W_start.*L))
            error("the starting point is not in Δ_w")
        end
        if iter == max_it
            error("reached maximum number of iterations")
        end
    end

    res = inner(W_start)

    # Sanity check: labor market clearing

    # Before
    # tol_lab = 1e-4 # 6 base
    # if !prod(isapprox.(sum(L_sec, dims = 2), L, atol = tol_lab))
    #     error("labor market clearing condition is not met: labor allocations are ", sum(L_sec, dims = 2))
    # end

    if max((abs.(sum(res.L_sec[1], dims = 2).-L)./L)...) > 1e-2 #1e-7
        # print(L_sec, P_sec, Q_sec, W_start)
        error("labor market clearing condition is not met: max(abs(model allocations - data allocations)/(data allocations)) are ", max((abs.(sum(res.L_sec[1], dims = 2).-L)./L)...))
    end

    # Normalize with respect to the numeraire (US labor)
    # W_us = W_start[ind_us]
    # W_start = W_start./W_us
    # res.P_f[1] = res.P_f[1]./W_us
    # res.P_sec[1] = res.P_sec[1]./W_us
    # res.R[1] = res.R[1]./W_us

    res.W = [W_start]

    return(res)
end

## Solve the model across years
function run_model(;seq_years, W_init_seq, κ = 0.1, max_it = 1e5, N, S, L_seq, Ā_seq, τ_seq, nx_seq, σ, ϵ, ϕ_seq, η, ρ, α_seq, θ, ind_us, P_start_seq)
    res_years = DataFrame()

    # Run model for each year
    for i_year in 1:length(seq_years)
        # println(i_year)
        res_years = [res_years; model_solve(;W_init = W_init_seq[i_year], κ = κ, max_it = max_it, N, S, L = L_seq[i_year], Ā = Ā_seq[i_year], τ = τ_seq[i_year], nx = nx_seq[i_year], σ, ϵ, ϕ = ϕ_seq[i_year], η, ρ, α = α_seq[i_year], θ, ind_us, P_start = P_start_seq[i_year])]
    end
    res_years.year = seq_years
    return(res_years)
end

# function run_model(;seq_years, W_init_seq, κ = 0.1, max_it = 1e5, N, S, L_seq, Ā_seq, τ_seq, nx_seq, σ, ϵ, ϕ_seq, η, ρ, α_seq, θ, ind_us)
#     res_years = DataFrame()
#
#     # Run model for each year
#     for year in seq_years
#         println(year)
#         i_year = year - seq_years[1] + 1
#         res_years = [res_years; model_solve(;W_init = W_init_seq[i_year], κ = κ, max_it = max_it, N, S, L = L_seq[i_year], Ā = Ā_seq[i_year], τ = τ_seq[i_year], nx = nx_seq[i_year], σ, ϵ, ϕ = ϕ_seq[i_year], η, ρ, α = α_seq[i_year], θ, ind_us)]
#     end
#     res_years.year = seq_years
#     return(res_years)
# end
