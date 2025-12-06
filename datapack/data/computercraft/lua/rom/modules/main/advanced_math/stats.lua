local stats = {}

-- Basic helpers
--add up all the datum
local function sum(data)
    local s = 0
    for _, v in pairs(data) do
        s = s + v
    end
    return s
end
--average of data set, remember that it is especially susceptible to outliers.
local function mean(data)
    local n = #data
    return n > 0 and sum(data) / n or nil
end
--gets smallest value of data set
local function min(data)
    table.sort(data)
    return data[1]
end
--gets largest value of data set
local function max(data)
    table.sort(data)
    return data[#data]
end
--square of stdev, true measure of how much the data varies
local function variance(data, is_sample)
    if #data == 0 then return nil end
    local sum_sq = 0
    for _, v in pairs(data) do
        sum_sq = sum_sq + (v - mean(data)) ^ 2
    end
    local denom = (is_sample and #data - 1 or #data)
    if denom <= 0 then return nil end
    return sum_sq / denom
end
--measure of amount of average variation from the mean, same as local
local function stdev(data, is_sample)
    local v = variance(data, is_sample)
    return v and math.sqrt(v) or nil
end
--size of the dataset(n)
stats.size = function(data)
    return #data
end
--see local vers
stats.sum = sum
--see local vers
stats.min = min
--see local vers
stats.max = max
--gets the distance between the max and min, especially vulnerable to outliers
stats.range = function(data)
    return min(data) and max(data) and max(data) - min(data) or nil
end
--see local vers
stats.mean = mean
--gets the middle value of the dataset(50th percentile). If set is even, averages the values in front and behind the 50th percentile.
stats.median = function(data)
    if #data == 0 then return nil end
    local t = { table.unpack(data) }
    table.sort(t)
    local n = #t
    if n % 2 == 1 then
        return t[(n + 1) / 2]
    else
        return (t[n / 2] + t[(n / 2) + 1]) / 2
    end
end
--finds the most frequent datum(or data) present in the set
stats.mode = function(data)
    local counts = {}
    local maxc = 0

    for _, v in pairs(data) do
        counts[v] = (counts[v] or 0) + 1
        if counts[v] > maxc then
            maxc = counts[v]
        end
    end
    if maxc == 0 then
        return {}
    end

    local modes = {}
    for v, c in pairs(counts) do
        if c == maxc then
            table.insert(modes, v)
        end
    end
    table.sort(modes)
    return modes
end
--see local vers
stats.variance = function(data, is_sample)
    return variance(data, is_sample ~= false)
end
--see local vers
stats.stdev = function(data, is_sample)
    return stdev(data, is_sample ~= false)
end
--measure of how likely the datum will vary from the mean, indicates precision
stats.sex = function(data)
    return #data > 0 and stdev(data, true) / math.sqrt(#data) or nil
end
--measures asymmetry of data around its mean
stats.skewness = function(data)
    local n = #data
    if n < 3 then return nil end
    local mu, sd = mean(data), stdev(data, true)
    if not sd or sd == 0 then return nil end
    local m3 = 0
    for _, v in pairs(data) do
        m3 = m3 + ((v - mu) / sd) ^ 3
    end
    return n / ((n - 1) * (n - 2)) * m3
end
--measure of skew, indicates how much peak or tail a distribution would have.
stats.kurtosis = function(data)
    local n = #data
    if n < 4 then return nil end
    local mu, sd = mean(data), stdev(data, true)
    if not sd or sd == 0 then return nil end
    local m4 = 0
    for _, v in pairs(data) do
        m4 = m4 + ((v - mu) / sd) ^ 4
    end
    return (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * m4
        - (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
end
--best for multiplicative data, like combining percentage growth rates.
stats.geometric_mean = function(data)
    local prod, n = 1, #data
    if n == 0 then return nil end
    for _, v in pairs(data) do
        if v <= 0 then return nil end
        prod = prod * v
    end
    return prod ^ (1 / n)
end
--best for average rates and ratios, or inverse proportionalities. DO NOT USE if you have 0 or negative values
stats.harmonic_mean = function(data)
    local sumr, n = 0, #data
    if n == 0 then return nil end
    for _, v in pairs(data) do
        if v == 0 then return nil end
        sumr = sumr + 1 / v
    end
    return n / sumr
end
--much more reliable measure of central tendency, clamps down outliers by removing a percentage of extremes.
stats.trimmed_mean = function(data, trim)
    trim = trim or 0.05
    local t = { table.unpack(data) }
    table.sort(t)
    local n = #t
    if n == 0 then return nil end
    local tn = math.floor(trim * n)
    local s = 0
    for i = tn + 1, n - tn do
        s = s + t[i]
    end
    local denom = n - 2 * tn
    if denom <= 0 then return nil end
    return s / denom
end
--median absolute deviation, much more reliable measure of variability when outliers are present, from the deviation
stats.mad_median = function(data)
    if #data == 0 then return nil end
    local med = stats.median(data)
    local devs = {}
    for _, v in pairs(data) do
        table.insert(devs, math.abs(v - med))
    end
    return stats.median(devs)
end
--much more reliable measure of central tendency, clamps down outliers by modifying extremes to be less extreme.
stats.winsor_mean = function(data, alpha)
    alpha = alpha or 0.05
    local t = { table.unpack(data) }
    table.sort(t)
    local n = #t
    if n == 0 then return nil end

    local lower = math.floor(alpha * n) + 1
    local upper = math.ceil((1 - alpha) * n)
    local winsor_t = { table.unpack(t) }

    local lower_bound = t[lower]
    local upper_bound = t[upper]
    for i = 1, lower - 1 do winsor_t[i] = lower_bound end
    for i = upper + 1, n do winsor_t[i] = upper_bound end

    return mean(winsor_t)
end

--returns a mean with certain weights given to specific data points.
stats.weighted_mean = function(data, weights)
    local n = #data
    if n == 0 or #weights ~= n then return nil end
    local num, den = 0, 0
    for i = 1, n do
        num = num + data[i] * weights[i]
        den = den + weights[i]
    end
    return den > 0 and num / den or nil
end
--represents how unequally distributed a dataset it, best explained by income but works for other sets too
stats.gini = function(data)
    local n = #data
    if n < 2 then return 0 end
    local t = { table.unpack(data) }
    table.sort(t)
    local mean_val = mean(t)
    if not mean_val or mean_val == 0 then return 0 end

    local cumsum = 0
    for i = 1, n do
        cumsum = cumsum + t[i]
    end

    local num = 0
    for i = 1, n do
        num = num + (2 * i - n - 1) * t[i]
    end
    return num / (n * cumsum)
end
--basic covariance, represents how much the two datasets will vary with each other
stats.covariance = function(x, y)
    local n = #x
    if n == 0 or #y ~= n then return nil end
    local mx, my = mean(x), mean(y)
    local cov = 0
    for i = 1, n do
        cov = cov + (x[i] - mx) * (y[i] - my)
    end
    if n <= 1 then return nil end
    return cov / (n - 1)
end
--basic r value, represents the strength and relationship of the covariances, much more interpretable
stats.correlation = function(x, y)
    local cov = stats.covariance(x, y)
    local sx, sy = stdev(x, true), stdev(y, true)
    if not cov or not sx or not sy or sx == 0 or sy == 0 then
        return 0
    end
    return cov / (sx * sy)
end
--finds the percentile of a value p given a dataset, sorts and places in between indices(or on one).
stats.percentile = function(data, p)
    local t = { table.unpack(data) }
    table.sort(t)
    local n = #t
    if n == 0 then return nil end
    local rank = p * (n - 1) + 1
    local f, c = math.floor(rank), math.ceil(rank)
    if f == c then
        return t[f]
    else
        return t[f] + (rank - f) * (t[c] - t[f])
    end
end
--gets the first and third quartiles. Quartiles are found by cutting the dataset in half using the median, and finding the median of those sets.
stats.quartiles = function(data)
    return {
        Q1 = stats.percentile(data, 0.25),
        Q3 = stats.percentile(data, 0.75)
    }
end

--gets IQR(distance between first and third quartiles)
stats.iqr = function(data)
    local q = stats.quartiles(data)
    if not q.Q1 or not q.Q3 then return nil end
    return q.Q3 - q.Q1
end
--lists all outliers in a dataset, outliers are considered as such if they are more than 150% of the IQR away from either the first or third quartiles
stats.outliers = function(data)
    local q = stats.quartiles(data)
    local iqr = stats.iqr(data)
    if not iqr then return {} end
    local low, high = q.Q1 - 1.5 * iqr, q.Q3 + 1.5 * iqr
    local outs = {}
    for _, v in pairs(data) do
        if v < low or v > high then
            table.insert(outs, v)
        end
    end
    return outs
end
--linearly finds a value from a table, simple as that
stats.find = function(tbl, value)
    for i=1, #tbl do
        if tbl[i] == value then
            return i
        end
    end
    return nil
end
--generates n quantitiative data points between a and b
stats.testdata = function(n, a, b)
    local result = {}
    a = a or 0
    b = b or 1
    for i = 1, n do
        result[i] = a + math.random() * (b - a)
    end
    return result
end
--minimizes squares to find the line that is the least distance squared from all data points
stats.linReg = function(x, y)
    local n = #x
    if n == 0 or #y ~= n then return nil end

    local mx, my = stats.mean(x), stats.mean(y)
    local num, den = 0, 0
    for i = 1, n do
        local dx = x[i] - mx
        num = num + dx * (y[i] - my)
        den = den + dx * dx
    end
    if den == 0 then return nil end

    local slope = num / den
    local intercept = my - slope * mx
    return { slope = slope, intercept = intercept }
end
--predicts a value for a given model at a given position
stats.linRegPred = function(model, xval)
    if not model then return nil end
    return model.slope * xval + model.intercept
end
--r^2 value, represents how much variation in variable y can be attributed to variable x
stats.r2 = function(x, y, model)
    local n = #y
    if n == 0 or not model then return nil end

    local ymean = stats.mean(y)
    local ssres, sstot = 0, 0
    for i = 1, n do
        local ypred = stats.linRegPred(model, x[i])
        ssres = ssres + (y[i] - ypred) ^ 2
        sstot = sstot + (y[i] - ymean) ^ 2
    end
    if sstot == 0 then
        return 1
    else
        return 1 - ssres / sstot
    end
end
--t tests check for statistical significance, to determine whether a relationship is due to chance or not.
--there are multiple types, but all return a p value. if the p value is greater than 0.05, that means the relationship is statistically significant.
--this one checks the data set against a standard t cumulative distribution frequency 
stats.tTest1Sample = function(data, mu0)
    local n = #data
    if n < 2 then return {t = 0, df = 0, p = 1} end
    local m = stats.mean(data)
    local se = stats.sex(data)
    if not se or se == 0 then return {t = 0, df = 0, p = 1} end
    local t = (m - mu0) / se
    local df = n - 1
    local p = 2 * (1 - stats.t_cdf(math.abs(t), df))
    return {t = t, df = df, p = p}
end
--this one checks the two data sets against each other(probably more useful)
stats.tTest2Samp = function(x, y, equal_var)
    local nx, ny = #x, #y
    if nx < 2 or ny < 2 then return {t = 0, df = 0, p = 1} end
    local mx, my = stats.mean(x), stats.mean(y)
    local vx, vy = stats.variance(x, true), stats.variance(y, true)
    equal_var = equal_var ~= false
    
    local se, df
    if equal_var then
        local pv = ((nx - 1) * vx + (ny - 1) * vy) / (nx + ny - 2)
        se = math.sqrt(pv * (1 / nx + 1 / ny))
        df = nx + ny - 2
    else
        se = math.sqrt(vx / nx + vy / ny)
        local num = (vx / nx + vy / ny)^2
        local den = (vx^2 / (nx^2 * (nx - 1))) + (vy^2 / (ny^2 * (ny - 1)))
        df = num / den
    end
    
    if se == 0 then return {t = 0, df = 0, p = 1} end
    local t = (mx - my) / se
    local p = 2 * (1 - stats.t_cdf(math.abs(t), df))
    return {t = t, df = df, p = p}
end
--like a two sample, but you check something before and after
stats.pairTTest = function(before, after)
    local n = #before
    if n < 2 or #after ~= n then return {t = 0, df = 0, p = 1} end
    local diffs = {}
    for i = 1, n do
        diffs[i] = before[i] - after[i]
    end
    return stats.t_test_one_sample(diffs, 0)
end
--checks a dataset against it's linear regression(basically does a few things at the same time)
stats.linRegTTest = function(x, y)
    local model = stats.linear_regression(x, y)
    if not model then return {t = 0, df = 0, p = 1} end
    
    local n = #x
    local slope_se = stats.stdev(y, true) / math.sqrt(stats.sum(function()
        local mx = stats.mean(x)
        local sxx = 0
        for i = 1, n do
            sxx = sxx + (x[i] - mx)^2
        end
        return sxx
    end))
    
    local t = model.slope / slope_se
    local df = n - 2
    local p = 2 * (1 - stats.t_cdf(math.abs(t), df))
    
    return {
        t = t,
        df = df,
        p = p,
        slope = model.slope,
        slope_se = slope_se,
        r_squared = stats.r_squared(x, y, model)
    }
end




return stats
