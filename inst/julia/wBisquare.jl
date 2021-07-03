using Statistics

"""
wBisquare weights updating 

wBisquare(y, yfit, w; iter = 2, wmin, to_upper = true)

# arguments
- `iter`    : not used
- `options` : currently ignored

# Bad points
- 1. under the yfit, in the growing season (yfit > 0.3 * A + ymin)
- 2. 

# Examples
"""
function wBisquare(y::Array{T,1}, yfit::Array{T,1}, w::Array{T2,1};
    # QC_flag;
    # iter::Integer = 2, 
    # to_upper = true, 
    wmin::Float64 = 0.05, 
    step::Float64 = 0.3, 
    trs_high = 0.7,
    trs_low  = 0.4,
    trs_bg = 0.2,
    options...) where {T <: AbstractFloat, T2 <: AbstractFloat}

    ymax = maximum(yfit)
    ymin = minimum(yfit)
    A = ymax - ymin;

    re     = yfit .- y
    re_abs = abs.(re)
    # println("re: $re")
    sc     = 6*median(re_abs)
    # println("sc", median(re_abs), ", ", sc)
    # 最保险的方法，获取每年的ylu，然后判断是ingrowing or ungrowing
    # println("threshold：high=", trs_high * A + ymin, ", low=", trs_low * A + ymin)
    y_grow = trs_high * A + ymin
    y_nongrow = trs_low * A + ymin
    y_bg = trs_bg * A + ymin

    I_bad_high = @.( (re > 0) & (yfit > y_grow) ) # middle of GS, upper envelope
    I_bad_low = @.( (re < 0) & (yfit < y_nongrow) & (yfit > y_bg) )

    I_good_high = @.( (re < 0) & (yfit > y_grow) ) 
    I_good_low  = @.( (re > 0) & (yfit < y_nongrow) & (yfit > y_bg) )
    
    I_bad = I_bad_high .| I_bad_low

    wnew = w; 
    bad = I_bad
    wnew[bad] = @.( (1 - (re_abs[bad]/sc)^2)^2 * w[bad]) # - step

    I_good = I_good_high .| I_good_low
    wnew[I_good] = wnew[I_good] .+ step

    ## 3 异常值赋予最低权重，改写对应的value
    # outlier不可纵容，即使是re < 0
    I_outlier = @. ( (re_abs > sc ) )# .& (QC_flag .!== Int(1)
    wnew[I_outlier] .= wmin
    wnew[wnew .< wmin] .= wmin

    y[I_outlier] = yfit[I_outlier]
    y[I_bad_high] .= yfit[I_bad_high]
    ## also need to change y to improve the upper envelope performance
    # println("inside wBisquare: ", sum(y))
    # wmax = 2.0
    # wnew[wnew .>= 1.0] .= 1.0
    wnew
end
