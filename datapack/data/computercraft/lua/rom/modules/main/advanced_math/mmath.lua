--- A grab bag of linear equation solvers. Useful for finding solution using linear algebra.
--
-- An introduction to Linear Algebra can be found on [Wikipedia][wiki].
--
-- [wiki]: https://en.wikipedia.org/wiki/Linear_algebra
--
-- If you are interested in using [CCSharp][ccsharp], here is the compatible [MMath.cs][ccsharp-mmath] file.
--
-- [ccsharp]: https://github.com/monkeymanboy/CCSharp
-- [ccsharp-mmath]: https://github.com/monkeymanboy/CCSharp/blob/master/src/CCSharp/AdvancedMath/MMath.cs
--
-- Credit goes to sans.9536 of the Minecraft Computer Mods discord for these functions and this file.
--
-- @module mmath
-- @author sans.9536

local expect = require "cc.expect"
local expect = expect.expect
local F = {}

--- Solves f(x) = g(x) over an interval by probing many starting points and using Newton-Raphson.
--
-- @tparam function f First function in the system (number -> number)
-- @tparam function g Second function in the system (number -> number)
-- @tparam number min Lower bound of the search interval
-- @tparam number max Upper bound of the search interval
-- @tparam number steps Number of probe seeds across the interval (integer >= 1)
-- @tparam number[opt] tol Tolerance forwarded to the root finder (defaults to 1e-10)
-- @tparam number[opt] closeThresh Distance threshold used to consider two roots identical
-- @treturn table Array (list) of root approximations found within [min, max]
-- @usage local roots = F.solveSysEq(math.sin, function(x) return 0 end, 0, 2 * math.pi, 100, 1e-8, 1e-6)
function F.solveSysEq(f, g, min, max, steps, tol, closeThresh)
    expect(1, f, "function")
    expect(2, g, "function")
    expect(3, min, "number")
    expect(4, max, "number")
    expect(5, steps, "number")
    expect(6, tol, "number", "nil")
    expect(7, closeThresh, "number", "nil")
    if min >= max then
        error("Minimum bound needs to be lesser than maximum bound!")
    end
    if steps < 1 then
        error("Number of Steps must be greater than or equal to one!")
    end

    local function h(f,g) 
        return function(x) return (f(x)-g(x)) end
    end
    local solutions = {}
    for i = 0, steps do
        local root = F.solveRoot(h(f,g), min + (max - min) * i / steps, tol)
        
        if root then
            local duplicate = false
            for _, sol in ipairs(solutions) do
                if math.abs(sol - root) < close_thresh then
                    duplicate = true
                    break
                end
            end
            if not duplicate and root >= min and root <= max then
                table.insert(solutions, root)
            end
        else
            local x = min + (max - min) * i / steps
            for j = 1,100 do
                if h(f,g)(x) < close_thresh then
                    local duplicate = false
                    for _, sol in ipairs(solutions) do
                        if math.abs(sol - x) < close_thresh then
                            duplicate = true
                            break
                        end
                    end
                    if not duplicate then
                        table.insert(solutions, x)
                    end
                    break
                else
                    x = x + (max - min) / (steps * 100)
                end
            end
        end
    end
    return solutions
end

--- Expand a set of values according to integer weights.
--
-- For each index i in `values`, `values[i]` is inserted into the returned
-- table `weights[i]` times. Both `values` and `weights` are treated as
-- array-like tables using matching indices.
-- @tparam table values Array/table of values to repeat
-- @tparam table weights Array/table of integer weights (same indexing as values)
-- @treturn table New array-like table with values repeated according to weights
-- @usage local expanded = F.weightedTable({"a","b"}, {3,1}) -- {"a","a","a","b"}
function F.weightedTable(values, weights)
    expect(1, values, "table")
    expect(2, weights, "table")
    if #values ~= #weights then
        error("The values and weights tables must have the same number of values!")
    end

    local result = {}
    for i,v in pairs(values) do
        local b = 1
        while weights[i] >= b do
            table.insert(result,v)
            b = b + 1
        end
    end
    return result
end

--- Shuffle (randomize) an array-like table in-place and return it.
--
-- This function mutates the input table `t` by repeatedly removing each
-- element and inserting it at a random position. It is a simple shuffle and
-- not an in-place Fisher–Yates implementation.
-- @tparam table t Array-like table to scramble (mutated)
-- @treturn table The same table, now scrambled
-- @usage F.scramble(myArray)
function F.scramble(t)
    expect(1, t, "table")

    for index, value in pairs(t) do
        local a = value
        table.remove(t, index)
        table.insert(t, math.random(1, #t + 1), a)
    end
    return t
end

--- Numerically integrate `f` on [a, b] using the trapezoid rule.
---
-- @tparam function f Function to integrate (callable: number -> number)
-- @tparam number n Number of subdivisions (integer > 0)
-- @tparam number a Lower bound
-- @tparam number b Upper bound
-- @tparam number[opt] init Optional initial accumulator (default 0)
-- @treturn number Approximation of the integral of `f` on [a, b]
-- @usage local I = F.integrateSimple(math.sin, 1000, 0, math.pi)
function F.integrateSimple(f, n, a, b, init)
    expect(1, f, "function")
    expect(2, n, "number")
    expect(3, a, "number")
    expect(4, b, "number")
    expect(5, init, "number", "nil")
    if n <= 0 then
        error("Number of Subdivisions must be greater than zero!")
    end
    if a >= b then
        error("The lower bound must be less than the upper bound!")
    end

    local width = (b - a) / n
    local sum = init or 0
    for i = 0, n - 1 do
        local x1 = a + i * width
        local x2 = a + (i + 1) * width
        sum = sum + (f(x1) + f(x2)) * width / 2
    end
    return sum
    
end

--- Numerically integrate `f` on [a, b] using Simpson-like parabolic rule.
--
-- If `n` is odd it is decremented to make it even. This method is generally
-- more accurate than the trapezoid rule for smooth functions.
-- @tparam function f Function to integrate (callable: number -> number)
-- @tparam number n Number of subdivisions (must be >= 2; adjusted to even if odd)
-- @tparam number a Lower bound
-- @tparam number b Upper bound
-- @tparam number[opt] init Optional initial accumulator (default 0)
-- @treturn number Simpson-style approximation of the integral
-- @usage local I = F.integrateComplex(math.sin, 1000, 0, math.pi)
function F.integrateComplex(f, n, a, b, init)
    expect(1, f, "function")
    expect(2, n, "number")
    expect(3, a, "number")
    expect(4, b, "number")
    expect(5, init, "number", "nil")
    if n <= 2 then
        error("Number of Subdivisions must be greater than two!")
    end
    if a >= b then
        error("The lower bound must be less than the upper bound!")
    end

    if n % 2 ~= 0 then
        n = n - 1
    end
    local width = (b - a) / n
    local sum = init or 0
    for i = 0, n / 2 - 1 do
        local x0 = a + i * 2 * width
        local x1 = a + (i * 2 + 1) * width
        local x2 = a + (i * 2 + 2) * width
        sum = sum + (f(x0) + 4 * f(x1) + f(x2)) * width / 3
    end
    return sum
    
end

--- Approximate the derivative (central difference) of a single-variable function.
--
-- Computes (f(x + h) - f(x - h)) / (2 * h). Use smaller `h` to approach the analytical
-- derivative but beware of numerical round-off for extremely small `h`.
-- @tparam function f Function to differentiate (callable: number -> number)
-- @tparam number x Point at which to evaluate the derivative
-- @tparam number[opt] h Step size (defaults to 1e-10)
-- @treturn number Numerical derivative approximation
-- @usage local d = F.ARC(math.sin, 1.0, 1e-6)
function F.ARC(f, x, h)
    expect(1, f, "function")
    expect(2, x, "number")
    expect(3, h, "number", "nil")

    h = h or 1e-10
    return (f(x + h) - f(x - h)) / (2 * h)
end

--- Find a root using Newton–Raphson starting from `x0`.
---
--- Attempts up to 500 iterations. The derivative is approximated using `F.ARC`.
--- On success returns the root (a number), otherwise returns `nil` to indicate failure.
--- @tparam function f Function for which to find a root (callable: number -> number)
--- @tparam number x0 Initial guess
--- @tparam number tol Convergence tolerance for changes in x
--- @treturn number|nil Root approximation or nil on failure
--- @usage local r = F.solveRoot(math.sin, 3.0, 1e-12)
function F.solveRoot(f, x0, tol)
    expect(1, f, "function")
    expect(2, x0, "number")
    expect(3, tol, "number")

    local x = x0
    for i = 1, 500 do
        local dfx = F.ARC(f, x)
        if math.abs(dfx) < 1e-12 then
            return nil
        end
        local x_new = x - f(x) / dfx
        if math.abs(x_new - x) < tol then
            return x_new
        end
        x = x_new
    end
    return nil
end

return F