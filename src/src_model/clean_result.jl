# clean_result.jl
# From the model simulations (equilibria), derive structural transformation pattern
# From the baseline model and data, get prices and trade shares to check the model fit

# Run the following, if the working directory of Julia is not the folder file is in and/or the environment is not loaded correctly.
#= cd(dirname(@__FILE__))
using Pkg
Pkg.activate("../") =#

using CSV, JLD2, DataFrames

# Load parameters and model simulation results
@load "../../output/model/model_input.jld2"

for i in 1:ncol(model_input)
    obj = names(model_input)[i]
    eval(Meta.parse(string(obj, " = model_input.", obj, "[1]")))
end

@load "../../output/model/models.jld2"

# Key variables: production (GDP) shares and consumption (expenditure) shares.
va_share(L_sec) = L_sec./sum(L_sec, dims = 2)
exp_share(P_sec, C_sec) = (P_sec.*C_sec)./sum((P_sec.*C_sec), dims = 2)

function result(dat, type_model, list_inds)

    dat.va_share = va_share.(dat.L_sec)
    dat.exp_share = exp_share.(dat.P_sec, dat.C_sec)

    res = DataFrame(year = repeat(1995:2018, inner = 67), country = repeat(list_countries, outer = 24), type = type_model)

    temp = DataFrame(vcat(dat.va_share...), :auto)
    temp2 = DataFrame(vcat(dat.exp_share...), :auto)

    rename!(temp, string.("va.", list_inds))
    rename!(temp2, string.("exp.", list_inds))

    res = hcat(res, temp, temp2)

    return(res)
end

list_inds = ["g", "bts", "hts"]
list_inds_alt = ["g", "cs", "ps"]
list_inds_gs = ["g", "s"]

# Baseline
str_chg = vcat(result(res_data, "data", list_inds), 
               result(res_base, "baseline", list_inds), 
               result(res_only_g, "count_only_g", list_inds),
               result(res_only_s, "count_only_s", list_inds),
               result(res_no, "count_no", list_inds))

# Head and Ries case
str_chg_hs = vcat(result(res_hs, "baseline", list_inds), 
                  result(res_only_g_hs, "count_only_g", list_inds),
                  result(res_only_s_hs, "count_only_s", list_inds),
                  result(res_hs_no, "count_no", list_inds))

# Model with non-tradable services
str_chg_no_s_trade = vcat(result(res_no_s_trade, "baseline", list_inds),
                          result(res_no_s_trade_no, "count_no", list_inds))

# Robustness

# 1. Two sector

get_result(res, res_no, list_inds) = vcat(result(res, "baseline", list_inds), result(res_no, "count_no", list_inds))

str_chg_gs = get_result(res_gs, res_gs_no, list_inds_gs)

# 2. Alternative three sector
str_chg_alt = get_result(res_alt, res_alt_no, list_inds_alt)

# 3. Trade elasticity for services
# Low
str_chg_low = get_result(res_low, res_low_no, list_inds)

# High
str_chg_high = get_result(res_high, res_high_no, list_inds)

# 4. Wedges approach
str_chg_wedge = get_result(res_wedge, res_wedge_no, list_inds)

# 5. Nested production function
str_chg_nest = get_result(res_nest, res_nest_no, list_inds)

# 6. No global imbalances (NX = 0)
str_chg_nx = get_result(res_nx, res_nx_no, list_inds)

# 7. Non-tradable CS case
str_chg_bts = get_result(res_bts, res_bts_no, list_inds)

# 8. No tau < 1 adjustment case
str_chg_orig = get_result(res_orig, res_orig_no, list_inds)

# Save the result in CSV file
# CSV.write("../../output/model/str_chg.csv", str_chg)

for i_model in ["str_chg", "str_chg_hs", "str_chg_no_s_trade", "str_chg_gs", "str_chg_alt", "str_chg_low", "str_chg_high", "str_chg_wedge", "str_chg_nest", "str_chg_nx", "str_chg_bts", "str_chg_orig"]
    eval(Meta.parse(string("CSV.write(\"../../output/model/", i_model, ".csv\", ", i_model, ")")))
end

# Model fit: compare prices and wages in the baseline and the data
# Since there is no preference and production wedge, the inversion method will not be providing perfect model fit.
# Input for the inversion: prices and import shares. So check the model fit in terms of the two variables.

# 1. Prices relative to US wage
function get_relp(dat, list_inds)
    get_wage(x) = x[ind_us]
    dat.W_us = get_wage.(dat.W)
    dat.P_sec_norm = deepcopy(dat.P_sec)
    dat.W_norm = deepcopy(dat.W)
    for i in 1:nrow(dat)
        dat.P_sec_norm[i] .= dat.P_sec[i]./dat.W_us[i]
        dat.W_norm[i] .= dat.W[i]./dat.W_us[i]
    end

    res = DataFrame(year = repeat(1995:2018, inner = 67), country = repeat(list_countries, outer = 24))

    temp_price = DataFrame(vcat(dat.P_sec_norm...), :auto)
    rename!(temp_price, string.("p.", list_inds))
    
    temp_wage = DataFrame(w = vcat(dat.W_norm...))

    res = hcat(res, temp_price, temp_wage)
    return(res)
end

relp_data = get_relp(res_data, list_inds)
relp_data.type = repeat(["data"], nrow(relp_data))

relp_base = get_relp(res_base, list_inds)
relp_base.type = repeat(["baseline"], nrow(relp_base))

CSV.write("../../output/model/model_fit_price.csv", vcat(relp_data, relp_base))

# 2. trade shares
function sh_countries(x, i_ind)
    x = DataFrame(x, :auto)
    rename!(x, list_countries)
    x.country = list_countries
    x.ind = repeat([i_ind], nrow(x))
    return(x)
end

function clean_sh_inner(dat_sh, list_inds)
    res = DataFrame()
    for (i, i_ind) in enumerate(list_inds)
        res = vcat(res, sh_countries(dat_sh[:,:,i], i_ind))
    end
    return(res)
end

function clean_sh(dat, list_inds)
    clean_sh_inner_2(x) = clean_sh_inner(x, list_inds)
    res = clean_sh_inner_2.(dat.sh)
    for i in 1:length(res)
        res[i][!, "year"] .= i + 1994
    end
    res = vcat(res...)
    return(res)
end

sh_data = clean_sh(res_data, list_inds)
sh_data[!, "type"] .= "data"
sh_base = clean_sh(res_base, list_inds)
sh_base[!, "type"] .= "baseline"

CSV.write("../../output/model/model_fit_share.csv", vcat(sh_data, sh_base))

# Labor productivity
lprd_inner(Q_sec, L_sec) = Q_sec./sum(L_sec)

function lprd(dat, type_model, list_inds)

    dat.lprd = lprd_inner.(dat.Q_sec, dat.L_sec)

    res = DataFrame(year = repeat(1995:2018, inner = 67), country = repeat(list_countries, outer = 24), type = type_model)

    temp = DataFrame(vcat(dat.lprd...), :auto)

    rename!(temp, string.("lprd.", list_inds))

    res = hcat(res, temp)

    return(res)
end

lprd_base = lprd(res_base, "baseline", list_inds)

CSV.write("../../output/model/lprd_base.csv", lprd_base)

print("Code ran without a problem")