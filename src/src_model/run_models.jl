# run_models.jl

# Run the following, if the working directory of Julia is not the folder file is in and/or the environment is not loaded correctly.
#= cd(dirname(@__FILE__))
using Pkg
Pkg.activate("../") =#

# Load required packages and files
using SpecialFunctions, Optim, LinearAlgebra, DataFrames, CSV, JLD2, Statistics, StatsBase

# Functions to solve the models (baseline case, nested production case)
include("model.jl")
include("model_nest.jl")

# Read model parameters and primitives
@load "../../output/model/model_input.jld2"

for i in 1:ncol(model_input)
    obj = names(model_input)[i]
    eval(Meta.parse(string(obj, " = model_input.", obj, "[1]")))
end

# Sanity checks: does the model provide perfect fit under the wedges approach? (with no adjustment for τ < 1)
fy_check = model_solve(;W_init = W_start[1], κ = 0.3, max_it = 1e5, N, S, L = L[1], Ā = A_wedge[1], τ = τ_orig[1], nx = nx[1], σ, ϵ, ϕ = ϕ_wedge[1], η, ρ, α = α_wedge[1], θ, ind_us, P_start = p_start[1])
println("Sanity check: in the wedges approach, model = data. The maximum gap between wages in the model and data in percentage is the following.")
@show max(abs.((fy_check.W[1] .- W_start[1])./W_start[1])...)*100

# Run the models

# 1. Data = wedges approach + no adjustment in τ < 1
seq_years = 1995:2018

run_wedge(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_wedge, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = ϕ_wedge, η, ρ, α_seq = α_wedge, θ, ind_us, P_start_seq = p_start)

res_data = run_wedge(τ_orig)

# 2. Baseline
run_base(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ, ind_us, P_start_seq = p_start)

res_base = run_base(τ)

# For counterfactuals, you need sequence of trade costs fixed at initial year
fix_tau = function(τ; S, fix_inds)
    τ_fixed = []
    for i in 1:24
        t_temp = Array{Float64}(undef,N,N,S)
        for s in 1:S
            if s ∈ fix_inds
                t_temp[:,:,s] .= τ[1][:,:,s]
            else 
                t_temp[:,:,s] .= τ[i][:,:,s]
            end
        end
        τ_fixed = [τ_fixed; [t_temp]]
    end
    return(τ_fixed)
end  
        
# 2-1. Counterfactual 1 (only goods liberalization)
τ_only_g = fix_tau(τ; S, fix_inds = 2:3)

res_only_g = run_base(τ_only_g)

# 2-2. Counterfactual 2 (only services liberalization)
τ_only_s = fix_tau(τ; S, fix_inds = [1])

res_only_s = run_base(τ_only_s)

# 2-3. Counterfactual 3 (no liberalization)
τ_no = fix_tau(τ; S, fix_inds = 1:3)

res_no = run_base(τ_no)

# 3. Head and Ries setup. This will be the key comparison.
res_hs = run_base(τ_hs)

# 3-1~3. Counterfactuals

τ_only_g_hs = fix_tau(τ_hs; S, fix_inds = 2:3)

res_only_g_hs = run_base(τ_only_g_hs)

τ_only_s_hs = fix_tau(τ_hs; S, fix_inds = [1])

res_only_s_hs = run_base(τ_only_s_hs)

τ_hs_no = fix_tau(τ_hs; S, fix_inds = 1:3)

res_hs_no = run_base(τ_hs_no)

# 4. Model with non-tradable services

# Function to change trade costs to infinity for some sectors
no_tau = function(τ, S, no_inds)
    τ_res = []
    for i in 1:24
        t_temp = Array{Float64}(undef,N,N,S)
        for s in 1:S
            if s ∈ no_inds
                t_temp[:,:,s] .= Inf
                for i in 1:N
                    t_temp[i,i,s] = 1
                end
            else
                t_temp[:,:,s] .= τ[i][:,:,s]
            end
        end
        τ_res = [τ_res; [t_temp]]
    end
    return(τ_res)
end  

#= run_no_s_trade(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_no_s_trade, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ, ind_us, P_start_seq = p_start) =#

run_no_s_trade(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_no_s_trade, τ_seq = tau, nx_seq = repeat([zeros(67)], 24), σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ, ind_us, P_start_seq = p_start)

τ_no_s_trade = no_tau(τ, S, 2:3)

τ_no_s_trade_no = fix_tau(τ_no_s_trade; S, fix_inds = 1:3)

res_no_s_trade = run_no_s_trade(τ_no_s_trade)

res_no_s_trade_no = run_no_s_trade(τ_no_s_trade_no)


## Robustness for the main exercise: to compare (proportionality index) vs (baseline - counterfactual 3)

# 1. Wedges approach
res_wedge = res_data

τ_no_orig = fix_tau(τ; S, fix_inds = 1:3)

res_wedge_no = run_wedge(τ_no_orig)

# 2. Alternative three sector definition

run_alt(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_alt, τ_seq = tau, nx_seq = nx, σ = σ_alt, ϵ = ϵ_alt, ϕ_seq = repeat([ϕ_alt], 24), η, ρ = ρ_alt, α_seq = repeat([α_alt], 24), θ, ind_us, P_start_seq = p_start)

res_alt = run_alt(τ_alt)

τ_alt_no = fix_tau(τ_alt; S, fix_inds = 1:3)

res_alt_no = run_alt(τ_alt_no)

# 3. Two sector (G and S)

run_gs(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S = S_gs, L_seq = L, Ā_seq = A_gs, τ_seq = tau, nx_seq = nx, σ = σ_gs, ϵ = ϵ_gs, ϕ_seq = repeat([ϕ_gs], 24), η = η_gs, ρ = ρ_gs, α_seq = repeat([α_gs], 24), θ = θ_gs, ind_us, P_start_seq = p_start_gs)

res_gs = run_gs(τ_gs)

τ_gs_no = fix_tau(τ_gs; S = S_gs, fix_inds = 1:2)

res_gs_no = run_gs(τ_gs_no)

# 4. Low theta for services & high theta for services

run_low(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_low, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ = θ_low, ind_us, P_start_seq = p_start)
run_high(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_high, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ = θ_high, ind_us, P_start_seq = p_start)

res_low = run_low(τ_low)

τ_low_no = fix_tau(τ_low; S, fix_inds = 1:3)

res_low_no = run_low(τ_low_no)

res_high = run_high(τ_high)

τ_high_no = fix_tau(τ_high; S, fix_inds = 1:3)

res_high_no = run_high(τ_high_no)

# 6. Nested version

run_nest(tau) = run_model_nest(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A_nest, τ_seq = tau, nx_seq = nx, σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ_inner, ρ_outer, α_inner_seq = repeat([α_inner], 24), α_outer_seq = repeat([α_outer], 24), θ = θ, ind_us, P_start_seq = p_start)

res_nest = run_nest(τ)

res_nest_no = run_nest(τ_no)

# 7. No NX

run_nx(tau) = run_model(;seq_years, W_init_seq = W_start, κ = 0.3, max_it = 1e5, N, S, L_seq = L, Ā_seq = A, τ_seq = tau, nx_seq = repeat([zeros(67)], 24), σ, ϵ, ϕ_seq = repeat([ϕ], 24), η, ρ, α_seq = repeat([α], 24), θ, ind_us, P_start_seq = p_start)

res_nx = run_nx(τ)

res_nx_no = run_nx(τ_no)

# 8. No BTS trade

τ_bts = no_tau(τ, 3, [2])

τ_bts_no = no_tau(τ_no, 3, [2])

res_bts = run_base(τ_bts)

res_bts_no = run_base(τ_bts_no)

# 9. No adjustment for τ < 1

res_orig = run_base(τ_orig)

τ_no_orig = fix_tau(τ_orig; S, fix_inds = 1:3)

res_orig_no = run_base(τ_no_orig)


# Save the result

jldsave("../../output/model/models.jld2"; 
        res_data, 
        res_base, res_only_g, res_only_s, res_no,
        res_hs, res_only_g_hs, res_only_s_hs, res_hs_no,
        res_no_s_trade, res_no_s_trade_no,
        res_wedge, res_wedge_no,
        res_alt, res_alt_no,
        res_gs, res_gs_no,
        res_low, res_low_no,
        res_high, res_high_no,
        res_nest, res_nest_no,
        res_nx, res_nx_no,
        res_bts, res_bts_no,
        res_orig, res_orig_no)
                                          
print("Code ran without a problem")