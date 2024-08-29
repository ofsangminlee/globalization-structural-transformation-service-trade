# inputs.jl
# Read model parameters/primitives in csv or RData format and convert them into julia arrays.

# Run the following, if the working directory of Julia is not the folder file is in and/or the environment is not loaded correctly.

#= cd(dirname(@__FILE__))
using Pkg
Pkg.activate("../") =#

using RData, DataFrames, JLD2, CSV

# Section 1. Parameters

N = 67 # Number of countries (row)
S = 3 # Number of sectors (column) in the order of G, BTS, HTS
S_gs = 2

# 1. Preference parameters

function get_pref(file_loc, list_eps)

    params = DataFrame(CSV.File(file_loc))
    
    get_param(x) = params.estimates[params.params .== x][1]
    
    σ = get_param("sigma")
    
    ϵ = get_param.(list_eps)
    
    return((σ, ϵ))
end

list_inds = ["g", "bts", "hts"]
list_inds_alt = ["g", "cs", "ps"]
list_inds_gs = ["g", "s"]

(σ, ϵ)= get_pref("../../output/estimated_params/sub_inc_elasticity.csv", string.("eps.", list_inds))

(σ_alt, ϵ_alt)= get_pref("../../output/estimated_params/sub_inc_elasticity_alt.csv", string.("eps.", list_inds_alt))

(σ_gs, ϵ_gs)= get_pref("../../output/estimated_params/sub_inc_elasticity_gs.csv", string.("eps.", list_inds_gs))

function get_phis(file_loc, list_inds; method = "avg")
    phis = DataFrame(CSV.File(file_loc))
    phis = unstack(phis, [:country, :year], :ind, :estimates)
    sort!(phis, [:country, :year]) # Don't forget to sort (already sorted, but just to be sure.)
    
    if method == "avg" # Fixed ϕ across years
        ϕ_avg = Array{Float64}(phis[phis.year .== "avg", list_inds])
        return(ϕ_avg)
            
    elseif method == "wedge" # Time-varying wedge ϕ
        ϕ_seq = []
        for i in 1995:2018
        phis_temp = phis[phis.year .== string("X", i), list_inds]
        ϕ_seq = [ϕ_seq; [Array{Float64}(phis_temp)]]
        end
        return(ϕ_seq)
    end
end

ϕ = get_phis("../../output/estimated_params/weight_util.csv", list_inds)
ϕ_wedge = get_phis("../../output/estimated_params/weight_util.csv", list_inds, method = "wedge")
ϕ_alt = get_phis("../../output/estimated_params/weight_util_alt.csv", list_inds_alt)
ϕ_gs = get_phis("../../output/estimated_params/weight_util_gs.csv", list_inds_gs)

# 2. Production parameters
η = [2.0, 2, 2]
η_gs = [2.0, 2]

function get_rho(file_loc, list_inds)
    rhos = DataFrame(CSV.File(file_loc))
    rhos = rhos[indexin(rhos.ind, list_inds),:]
    ρ = Array(rhos.rho)
    return(ρ)
end

ρ = get_rho("../../output/estimated_params/rho.csv", list_inds)
ρ_alt = get_rho("../../output/estimated_params/rho_alt.csv", list_inds_alt)
ρ_gs = get_rho("../../output/estimated_params/rho_gs.csv", list_inds_gs)

ρ_inner = get_rho("../../output/estimated_params/rho_nest_inner.csv", list_inds)

temp = DataFrame(CSV.File("../../output/estimated_params/rho_nest_outer.csv"))
temp = temp[temp.method .== "average", :]
temp = temp[indexin(temp.ind, list_inds), :]
ρ_outer = Array(temp.rho)

# α: production weights

function get_alphas(file_loc, list_inds; method = "avg")
    alphas = DataFrame(CSV.File(file_loc))
    sort!(alphas, [:country, :year, :ind])
    names_industries = string.("alpha.", [list_inds..., "v"])
    
    S = length(list_inds)

    if method == "avg" # Fixed α across years
        alphas_avg = alphas[alphas.year .== "avg",:]
        α_avg = Array{Float64}(undef,N,S+1,S) # country, origin sector (last sector: value-added), destination sector
        for i in 1:S
            α_avg[:,:,i] .= alphas_avg[alphas_avg.ind .== list_inds[i], names_industries]
        end
        return(α_avg)
    elseif method == "wedge" # Time-varying wedge α
        α_seq = []
        for i in 1995:2018  
            α_temp = Array{Float64}(undef,N,S+1,S) # country, origin industry (value-added: the last sector), destination industry
            alphas_inner = alphas[alphas.year .== string("X",i),:]
            for i in 1:S
                α_temp[:,:,i] .= alphas_inner[alphas_inner.ind .== list_inds[i], names_industries]
            end
            α_seq = [α_seq; [α_temp]]
        end
        return(α_seq)
    end
end

α = get_alphas("../../output/estimated_params/alphas.csv", list_inds)
α_wedge = get_alphas("../../output/estimated_params/alphas.csv", list_inds, method = "wedge")
α_alt = get_alphas("../../output/estimated_params/alphas_alt.csv", list_inds_alt)
α_gs = get_alphas("../../output/estimated_params/alphas_gs.csv", list_inds_gs)

α_nest = get_alphas("../../output/estimated_params/alphas_nest.csv", list_inds)
α_inner = α_nest[:,1:3,:]
α_outer = α_nest[:,4,:]

# Trade elasticity

θ = [4.0, 4, 4]
θ_gs = [4.0, 4]
θ_low = [4.0, 2, 2]
θ_high = [4.0, 6, 6]



# Section 2. Primitives: time-varying objects (from data and calibration)

# 1. Labor endowment
labor = DataFrame(CSV.File("../../output/cleaned_data/pwt_variables.csv"))
sort!(labor, :country)
L = []
for i in string.("X", 1995:2018)
    L_temp = labor[labor.year .== i, :emp]
    global L = [L; [L_temp]]
end

# 2. Trade costs

# Sanity check for tau matrix for a year
function tau_sanity(dat)
    list_country_ordered = sort(dat.country)
    if !(dat.country == list_country_ordered) | !(names(dat)[2:length(names(dat))] == list_country_ordered)
        error("Countries are not alphabetically ordered")
    end
end

# List of countries (alphabetically ordered)
list_countries = sort(unique(labor.country))

# Function to get trade costs
function get_tau(file_loc, name_dataframe, list_inds, list_countries)
    tau = load(file_loc)[name_dataframe]
    S = length(list_inds)
    τ_seq = []
    for i in 1:24 # 1995 to 2018
        t_temp = Array{Float64}(undef,N,N,S)
        for (j, ind) in enumerate(list_inds)
            tau_sanity(tau[ind][j])
            t_temp[:,:,j] .= tau[ind][i][:, list_countries]
        end
        τ_seq = [τ_seq; [t_temp]]
    end
    return(τ_seq)
end

τ = get_tau("../../output/estimated_params/tau.RData", "tau", list_inds, list_countries)
τ_hs = get_tau("../../output/estimated_params/tau_head_ries.RData", "tau.hs", list_inds, list_countries)

τ_orig = get_tau("../../output/estimated_params/tau_orig.RData", "tau.orig", list_inds, list_countries)

τ_alt = get_tau("../../output/estimated_params/tau_alt.RData", "tau.alt", list_inds_alt, list_countries)
τ_gs = get_tau("../../output/estimated_params/tau_gs.RData", "tau.gs", list_inds_gs, list_countries)
τ_low = get_tau("../../output/estimated_params/tau_low.RData", "tau.low", list_inds, list_countries)
τ_high = get_tau("../../output/estimated_params/tau_high.RData", "tau.high", list_inds, list_countries)

# 3. Productivities

# Function to get productivities
function get_prd(file_loc, list_inds)
    prd = DataFrame(CSV.File(file_loc))
    sort!(prd, [:year, :country])

    S = length(list_inds)
    
    A_seq = []

    for year in string.("X", 1995:2018)
        prd_temp = prd[prd.year .== year,:]
        A_temp = Array{Float64}(undef, N,S)
        for (i, ind) in enumerate(list_inds)
            A_temp[:,i] .= prd_temp[prd_temp.ind .== ind, "prd"]
        end
        A_seq = [A_seq; [A_temp]]
    end

    return(A_seq)

end

A = get_prd("../../output/estimated_params/prd.csv", list_inds)
A_alt = get_prd("../../output/estimated_params/prd_alt.csv", list_inds_alt)
A_gs = get_prd("../../output/estimated_params/prd_gs.csv", list_inds_gs)
A_low = get_prd("../../output/estimated_params/prd_low.csv", list_inds)
A_high = get_prd("../../output/estimated_params/prd_high.csv", list_inds)
A_wedge = get_prd("../../output/estimated_params/prd_wedge.csv", list_inds)
A_nest = get_prd("../../output/estimated_params/prd_nest.csv", list_inds)
A_no_s_trade = get_prd("../../output/estimated_params/prd_no_s_trade.csv", list_inds)

# 4. NX terms (nominal NX)
NX_data = DataFrame(CSV.File("../../output/estimated_params/nx.csv"))
sort!(NX_data, [:year, :country])
NX_data_seq = []
for year in string.("X", 1995:2018)
    NX_data_temp = NX_data[NX_data.year .== year, :nx]
    global NX_data_seq = [NX_data_seq; [NX_data_temp]]
end

# Wages
W_data = DataFrame(CSV.File("../../output/cleaned_data/gdp_per_capita.csv"))
sort!(W_data, [:country, :year])

W_data_seq = []
for year in string.("X", 1995:2018)
    W_data_temp = W_data[W_data.year .== year,:gdppc]
    global W_data_seq = [W_data_seq; [W_data_temp]]
end

# NX to nx (in terms of US wage)
ind_us = findall(list_countries .== "USA")[1]
nx = []
for i in 1:length(NX_data_seq)
    global nx = [nx; [NX_data_seq[i]./(W_data_seq[i][ind_us])]]
end



# Section 3. Starting points (wages and prices) for model simulation
# This will make the code start searching for counterfactual equilibrium from closer starting points.

# 1. Wages
# Note: in the equilibria, prices are normalized so that sum(W*L) = 1.
conv = DataFrame(year = string.("X", 1995:2018), conv = zeros(2018-1994))
for i_year in string.("X", 1995:2018)
    conv.conv[conv.year .== i_year] .= sum(W_data.gdp[W_data.year .== i_year])
end

W_start = deepcopy(W_data_seq)
for i in 1:24
    W_start[i] .= W_start[i]./conv.conv[conv.year .== string("X", i + 1994)]
end

# 2. Prices
# Note: for prices data, one can check the units of prices by running, for example, min(p_start_seq). Given the price units, one can think about precision for iteration routines for P and R.

p_data = DataFrame(CSV.File("../../output/cleaned_data/gopl.csv"))

function get_p_start(file_loc, list_inds)
    p_data = DataFrame(CSV.File(file_loc))
    p_data = p_data[:, [:country, :ind, :year, :pl]]
    p_data = leftjoin(p_data, conv, on = :year)
    sort!(p_data, [:country, :ind, :year])  
    p_data[:,:p_start] .= p_data.pl./p_data.conv

    S = length(list_inds)

    p_start_seq = []
    
    for year in string.("X", 1995:2018)
        p_start_temp = p_data[p_data.year .== year,:]
        p_temp = Array{Float64}(undef, N, S)
        for (i, ind) in enumerate(list_inds)
            p_temp[:,i] .= p_start_temp[p_start_temp.ind .== ind, :p_start]
        end
        p_start_seq = [p_start_seq; [p_temp]]
    end

    return(p_start_seq)
end

p_start = get_p_start("../../output/cleaned_data/gopl.csv", list_inds)
p_start_alt = get_p_start("../../output/cleaned_data/gopl_alt.csv", list_inds_alt)
p_start_gs = get_p_start("../../output/cleaned_data/gopl_gs.csv", list_inds_gs)

# Save the model inputs
model_input = DataFrame(N = [N], S = [S], S_gs = [S_gs],
                        list_inds = [list_inds], list_inds_alt = [list_inds_alt], list_inds_gs = [list_inds_gs],
                        list_countries = [list_countries], ind_us = [ind_us],
                        σ = [σ], ϵ = [ϵ], σ_alt = [σ_alt], ϵ_alt = [ϵ_alt], σ_gs = [σ_gs], ϵ_gs = [ϵ_gs],
                        ϕ = [ϕ], ϕ_wedge = [ϕ_wedge], ϕ_alt = [ϕ_alt], ϕ_gs = [ϕ_gs],
                        η = [η], η_gs = [η_gs],
                        ρ = [ρ], ρ_alt =[ρ_alt], ρ_gs = [ρ_gs], ρ_inner = [ρ_inner], ρ_outer = [ρ_outer],
                        α = [α], α_wedge = [α_wedge], α_alt = [α_alt], α_gs = [α_gs], α_inner = [α_inner], α_outer = [α_outer],
                        θ = [θ], θ_low = [θ_low], θ_high = [θ_high], θ_gs = [θ_gs],
                        τ = [τ], τ_hs = [τ_hs], τ_orig = [τ_orig], τ_alt = [τ_alt], τ_gs = [τ_gs], τ_low = [τ_low], τ_high = [τ_high],
                        A = [A], A_wedge = [A_wedge], A_alt = [A_alt], A_gs = [A_gs], A_low = [A_low], A_high = [A_high], A_nest = [A_nest], A_no_s_trade = [A_no_s_trade],
                        L = [L],
                        W_start = [W_start], nx = [nx], p_start = [p_start], p_start_alt = [p_start_alt], p_start_gs = [p_start_gs])

@save "../../output/model/model_input.jld2" model_input

print("Code ran without a problem")