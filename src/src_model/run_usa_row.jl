# run_usa_row.jl
# Run two-country (USA-ROW) model 
# Note: country name for ROW was set to ZZZ to disambiguate it from ROW (World - 66 countries) vs ROW (World - USA)

# Run the following, if the working directory of Julia is not the folder file is in and/or the environment is not loaded correctly.
#= cd(dirname(@__FILE__))
using Pkg
Pkg.activate("../") =#

# Read inputs for the two country model
# Change csv or RData files into julia arrays.

using RData, DataFrames, JLD2, CSV
using SpecialFunctions, Optim, LinearAlgebra, DataFrames
using Statistics, StatsBase

## Parameters

N = 2 # Number of countries (row)
S = 3 # Number of sectors (column) in the order of G, BTS, HTS

# Preference parameters (estimated without US)
params = DataFrame(CSV.File("../../output/estimated_params/sub_inc_elasticity_wo_usa.csv"))

get_param(x) = params.estimates[params.params .== x]

σ = get_param("sigma")[1]
ϵ = [get_param("eps.g")[1], get_param("eps.bts")[1], get_param("eps.hts")[1]]

phis = DataFrame(CSV.File("../../output/estimated_params/weight_util_zzz.csv"))
sort!(phis, [:country, :year]) # Don't forget to sort

# Fixed ϕ across years
ϕ_zzz = Array(phis[phis.year .== "X1995", [:g, :bts, :hts]])

# Production parameters
η = [2.0, 2, 2]

rhos = DataFrame(CSV.File("../../output/estimated_params/rho_wo_usa.csv"))
rhos.ind == ["g", "bts", "hts"]
ρ = Array(rhos.rho)

# Fixed α across years

alphas = DataFrame(CSV.File("../../output/estimated_params/alphas_zzz.csv"))
sort!(alphas, [:country, :year, :ind])
alphas_zzz = alphas[alphas.year .== "X1995",:]
names_industries = string.("alpha.", ["g", "bts", "hts", "v"])

α_zzz = Array{Float64}(undef,N,S+1,S) # country, origin sector (last sector: value-added), destination sector
α_zzz[:,:,1] .= alphas_zzz[alphas_zzz.ind .== "g", names_industries]
α_zzz[:,:,2] .= alphas_zzz[alphas_zzz.ind .== "bts", names_industries]
α_zzz[:,:,3] .= alphas_zzz[alphas_zzz.ind .== "hts", names_industries]

θ = [4.0, 4, 4]

## Primitives: time-varying objects (from data and calibration)

# Labor endowment
labor = DataFrame(CSV.File("../../output/cleaned_data/pwt_zzz.csv"))
sort!(labor, :country)
L_seq = []
for i in string.("X", 1995:2018)
    L_temp = labor[labor.year .== i, :emp]
    global L_seq = [L_seq; [L_temp]]
end

# Trade costs
tau = load("../../output/estimated_params/tau_zzz.RData")["tau"]

# Sanity check
tttemp = tau["g"][1]
tttemp.country == sort(tttemp.country)
tttemp.country == names(tttemp)[2:length(names(tttemp))]
list_countries = tttemp.country

τ_seq = []

for i in 1:24 # 1995 to 2018
    t_temp = Array{Float64}(undef, N,N,S)
    t_temp[:,:,1] .= tau["g"][i][:, list_countries]
    t_temp[:,:,2] .= tau["bts"][i][:, list_countries]
    t_temp[:,:,3] .= tau["hts"][i][:, list_countries]
    global τ_seq = [τ_seq; [t_temp]]
end

# Productivities
product = DataFrame(CSV.File("../../output/estimated_params/productivity_zzz.csv"))
sort!(product, [:year, :country])

function get_A(product, var_name)
    A_res = []
    for year in 1995:1995
        product_temp = product[product.year .== year,:]
        A_temp = Array{Float64}(undef, N,S)
        A_temp[:,1] .= product_temp[product_temp.ind .== "g", var_name]
        A_temp[:,2] .= product_temp[product_temp.ind .== "bts", var_name]
        A_temp[:,3] .= product_temp[product_temp.ind .== "hts", var_name]
        A_res = [A_res; [A_temp]]
    end
    return(A_res)
end

A_fy = get_A(product, "productivity")

# USA productivity data
product_usa = DataFrame(CSV.File("../../output/estimated_params/prod_usa_data.csv"))

function get_A_usa(product, var_name)
    A_res = []
    for year in 1995:2018
        product_temp = product[product.year .== year,:]
        A_temp = Array{Float64}(undef, 1,S)
        A_temp[:,1] .= product_temp[product_temp.ind .== "g", var_name]
        A_temp[:,2] .= product_temp[product_temp.ind .== "bts", var_name]
        A_temp[:,3] .= product_temp[product_temp.ind .== "hts", var_name]
        A_res = [A_res; [A_temp]]
    end
    return(A_res)
end

A_usa = get_A_usa(product_usa, "productivity")

# NX terms (nominal NX)
NX_data = DataFrame(CSV.File("../../output/estimated_params/nx.csv"))
sort!(NX_data, [:year, :country])
NX_data_seq = []
for year in string.("X", 1995:2018)
    NX_data_temp = NX_data[NX_data.year .== year, :nx]
    global NX_data_seq = [NX_data_seq; [NX_data_temp]]
end

# Wages data to get list of countries and US GDP
W_data = DataFrame(CSV.File("../../output/cleaned_data/gdp_per_capita.csv"))
sort!(W_data, [:country, :year])
list_countries_all = unique(W_data.country)

sort!(list_countries_all)

W_data_seq = []
for year in string.("X", 1995:2018)
    W_data_temp = W_data[W_data.year .== year,:gdppc]
    global W_data_seq = [W_data_seq; [W_data_temp]]
end

# NX to nx (in terms of us labor)
ind_us = findall(list_countries_all .== "USA")[1]
nx_seq = []
for i in 1:length(NX_data_seq)
    temp = NX_data_seq[i][ind_us]./W_data_seq[i][ind_us]
    global nx_seq = [nx_seq; [[temp, -temp]]]
end

#= model_input = DataFrame(N = [N],S = [S], list_countries = [list_countries], ind_us = [ind_us], σ = [σ], ϵ = [ϵ], ϕ_zzz = [ϕ_zzz], η = [η], ρ = [ρ], α_zzz = [α_zzz], θ = [θ], L_seq = [L_seq], τ_seq = [τ_seq], A_fy = [A_fy], A_usa = [A_usa], nx_seq = [nx_seq]) =#

# Run two-country model
include("model.jl")

# First year check

# Initial wage vector
W_init_seq = []
for i in 1:24
    global W_init_seq = [W_init_seq; [repeat([1.0./sum(L_seq[i])], 2)]]
end

ind_us = 1

fy_check = model_solve(;W_init = repeat([1.0./sum(L_seq[1])], 2), κ = 0.3, max_it = 1e5, N, S, L = L_seq[1], Ā = A_fy[1], τ = τ_seq[1], nx = nx_seq[1], σ, ϵ, ϕ = ϕ_zzz, η, ρ, α = α_zzz, θ, ind_us, P_start = ones(N,S))

fy_check.P_sec[1]
qwer = fy_check.L_sec[1]
qwer./sum(qwer, dims = 2) # In the data, USA first-year GDP share is (22, 39, 40%) for (G, BTS, HTS)

# Get productivity sequence for ROW(ZZZ)

function price_gap(i_year, A_zzz)
    A_inner = [A_usa[i_year]; transpose(A_zzz)]
    temp = model_solve(;W_init = repeat([1.0./sum(L_seq[i_year])], 2), κ = 0.3, max_it = 1e5, N, S, L = L_seq[i_year], Ā = A_inner, τ = τ_seq[i_year], nx = nx_seq[i_year], σ, ϵ, ϕ = ϕ_zzz, η, ρ, α = α_zzz, θ, ind_us, P_start = ones(N,S))
    return(sum((temp.P_sec[1][1,:].-temp.P_sec[1][2,:]).^2))
end

A_zzz_seq = [A_fy[1][2,:]]

for i in 2:24
    qq(A_zzz) = price_gap(i, A_zzz)
    temp = optimize(qq, A_zzz_seq[i-1], LBFGS(), Optim.Options(g_tol = 1e-30))
    global A_zzz_seq = [A_zzz_seq; [temp.minimizer]]
end

A_usa_zzz_seq = []
for i in 1:24
    temp = [A_usa[i]; transpose(A_zzz_seq[i])]
    global A_usa_zzz_seq = [A_usa_zzz_seq; [temp]]
end

@save "../../output/model/usa_zzz_a.jl2" A_usa_zzz_seq

seq_years = 1995:2018
res_tr = run_model(;seq_years, W_init_seq, κ = 0.3, max_it = 1e5, N, S, L_seq, Ā_seq = A_usa_zzz_seq, τ_seq, nx_seq, σ, ϵ, ϕ_seq = repeat([ϕ_zzz], 24), η, ρ, α_seq = repeat([α_zzz],24), θ, ind_us, P_start_seq = repeat([ones(N,S)],24))

res_tr.L_sec[1]./sum(res_tr.L_sec[1], dims = 2)
res_tr.L_sec[24]./sum(res_tr.L_sec[24], dims = 2)

# 2. Model with non-tradable services
τ_seq_s_aut = []
for i in 1:24
    t_temp = Array{Float64}(undef, N,N,S)
    t_temp[:,:,1] .= τ_seq[i][:,:,1]
    t_temp[:,:,2] .= Inf
    t_temp[:,:,3] .= Inf
    for j in 1:N
        t_temp[j,j,2] = 1
        t_temp[j,j,3] = 1
    end
    global τ_seq_s_aut = [τ_seq_s_aut; [t_temp]]
end

t_aut = Array{Float64}(undef, N,N,S)
t_aut[:,:,:] .= Inf
for i in 1:3
    for j in 1:N
        t_aut[j,j,i] = 1
    end
end

res_aut = run_model(;seq_years, W_init_seq, κ = 0.3, max_it = 1e5, N, S, L_seq, Ā_seq = A_usa_zzz_seq, τ_seq = repeat([t_aut], 24), nx_seq = repeat([[0,0]], 24), σ, ϵ, ϕ_seq = repeat([ϕ_zzz], 24), η, ρ, α_seq = repeat([α_zzz],24), θ, ind_us, P_start_seq = repeat([ones(N,S)],24))

res_s_aut = run_model(;seq_years, W_init_seq, κ = 0.3, max_it = 1e5, N, S, L_seq, Ā_seq = A_usa_zzz_seq, τ_seq = τ_seq_s_aut, nx_seq, σ, ϵ, ϕ_seq = repeat([ϕ_zzz], 24), η, ρ, α_seq = repeat([α_zzz],24), θ, ind_us, P_start_seq = repeat([ones(N,S)],24))

va_share(L_sec) = L_sec./sum(L_sec, dims = 2)

exp_share(P_sec, C_sec) = (P_sec.*C_sec)./sum((P_sec.*C_sec), dims = 2)

function result(dat)
    dat.va_share = va_share.(dat.L_sec)
    dat.exp_share = exp_share.(dat.P_sec, dat.C_sec)

    res = DataFrame(year = repeat(1995:2018, inner = 2), country = repeat(list_countries, outer = 24))
    hcat(res, DataFrame(vcat(dat.va_share...), :auto))

    temp = DataFrame(vcat(dat.va_share...), :auto)
    rename!(temp, string.("va.", ["g", "bts", "hts"]))

    temp2 = DataFrame(vcat(dat.exp_share...), :auto)
    rename!(temp2, string.("exp.", ["g", "bts", "hts"]))

    temp3 = DataFrame(util = vcat(dat.C_f...))

    res = hcat(res, temp, temp2, temp3)
    return(res)
end

result_tr = result(res_tr)
result_no_s = result(res_s_aut)

CSV.write("../../output/model/usa_result_tr.csv", result_tr)
CSV.write("../../output/model/usa_result_no_s.csv", result_no_s)

print("Code ran without a problem")