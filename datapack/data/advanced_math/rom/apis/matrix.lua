--- A basic matrix type and common matrix operations. This may be useful
-- when working with linear algebra, transformations, and mathematical computations.
--
-- An introduction to matrices can be found on [Wikipedia][wiki].
--
-- [wiki]: https://en.wikipedia.org/wiki/Matrix_(mathematics)
--
-- @module matrix

--- Constructors
--
-- @section Constructors

--- Constructs a new matrix of rows by columns, filling it using the provided function or scalar.
--
-- @tparam number rows The number of rows in the matrix
-- @tparam number columns The number of columns in the matrix
-- @tparam function|number|nil func A function taking (row, column) to generate values, or a scalar to fill all elements
-- @treturn Matrix A new matrix
-- @usage m = matrix.new(3, 3, function(r, c) return r + c end)
-- @usage m = matrix.new(2, 4, 5) -- fills all elements with 5
-- @usage m = matrix.new(2, 2) -- fills all elements with 1
-- @export
function new(rows, columns, func)
    expect(1, rows, "number", "nil")
    expect(2, columns, "number", "nil")
    expect(3, func, "function", "number", "nil")

    local m = {}
    m.rows = rows or 1
    m.columns = columns or 1
    for r = 1, rows do
        m[r] = {}
        for c = 1, columns do
            if type(func) == "function" then
                m[r][c] = func(r, c)
            elseif type(func) == "number" then
                m[r][c] = func
            else
                m[r][c] = 1
            end
        end
    end
    return setmetatable(m, metatable)
end

--- Constructs a matrix from a 2D array (table of tables).
--
-- @tparam table arr A 2D array representing the matrix data
-- @treturn Matrix A new matrix
-- @usage m = matrix.from2DArray({{1, 2}, {3, 4}})
-- @export
function from2DArray(arr)
    expect(1, arr, "table")
    if getmetatable(arr) ~= nil then
        error("Invalid Argument! Takes a 2D array!")
    end

    return new(#arr, #arr[1], function(r, c) return arr[r][c] or 0 end)
end

--- Constructs a matrix from a vector, as either a row or column matrix.
--
-- @tparam table v The vector to convert
-- @tparam boolean row Whether to create a row matrix (true) or column matrix (false). Defaults to true.
-- @treturn Matrix A new matrix representing the vector
-- @usage m = matrix.fromVector(vector.new(1, 2, 3), true) -- row matrix
-- @usage m = matrix.fromVector(vector.new(1, 2, 3), false) -- column matrix
-- @export
-- @see https://tweaked.cc/module/vector.html vector
function fromVector(v, row)
    expect(1, v, "Vector")
    expect(2, row, "boolean", "nil")

    row = row or true
    local m = {}
    if row then
        m[1] = {v.x, v.y, v.z}
    else
        m[1] = {v.x}
        m[2] = {v.y}
        m[3] = {v.z}
    end
    return from2DArray(m)
end

--- Constructs a rotation matrix from a quaternion.
--
-- @tparam table q The quaternion to convert
-- @treturn Matrix A new 3x3 rotation matrix
-- @usage m = matrix.fromQuaternion(quaternion.new(1, vector.new(0, 0, 0)))
-- @export
-- @see quaternion
function fromQuaternion(q)
    if not quaternion then
        error("Quaternion API is not loaded!")
    end
    expect(1, q, "Quaternion")

    q = q:normalize()
    local w = q.a
    local x = q.v.x
    local y = q.v.y
    local z = q.v.z

    local m = {
        {1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y)},
        {2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x)},
        {2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)}
    }
    return from2DArray(m)
end

--- Constructs an identity matrix of given dimensions.
--
-- @tparam number rows The number of rows
-- @tparam number columns The number of columns
-- @treturn Matrix A new identity matrix
-- @usage m = matrix.identity(3, 3)
-- @export
function identity(rows, columns)
    return new(rows, columns)
end

--- A matrix, with dimensions `rows` x `columns`.
--
-- This is suitable for representing linear transformations, systems of equations,
-- and general numerical computations.
--
-- @type Matrix

local expect = require "cc.expect"
local expect = expect.expect

local matrix = {
    --- The number of rows in the matrix.
    -- @field rows
    -- @tparam number rows

    --- The number of columns in the matrix.
    -- @field columns
    -- @tparam number columns

    --- Adds two matrices together, or adds a scalar to all elements.
    -- Supports broadcasting with row vectors and column vectors.
    --
    -- @tparam Matrix self The first matrix to add
    -- @tparam Matrix|number other The second matrix, scalar, or vector to add
    -- @treturn Matrix The resulting matrix
    -- @usage m1:add(m2)
    -- @usage m1 + m2
    -- @usage m + 5
    add = function(self, other)
        expect(1, self, "number", "Matrix")
        expect(2, other, "number", "Matrix")

        if type(self) == "number" then
            return other + self
        end
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                if type(other) == "number" then
                    m[r][c] = self[r][c] + other
                elseif type(other) == "table" then
                    if other.rows == self.rows and other.columns == 1 then -- rows vector
                        m[r][c] = self[r][c] + other[r][1]
                    elseif other.columns == self.columns and other.rows == 1 then -- columns vector
                        m[r][c] = self[r][c] + other[1][c]
                    elseif other.rows == self.rows or other.columns == self.columns then
                        m[r][c] = self[r][c] + other[r][c]
                    else
                        error("Invalid Argument! Takes a scalar value, a vector matrix or another matrix of the same dimensions!")
                    end
                else
                    error("Invalid Argument! Takes a scalar value, a vector matrix or another matrix of the same dimensions!")
                end
            end
        end
        return from2DArray(m)
    end,

    --- Subtracts two matrices, or subtracts a scalar from all elements.
    --
    -- @tparam Matrix self The matrix to subtract from
    -- @tparam Matrix|number other The matrix or scalar to subtract
    -- @treturn Matrix The resulting matrix
    -- @usage m1:sub(m2)
    -- @usage m1 - m2
    sub = function(self, other)
        return self + (-other)
    end,

    --- Multiplies a matrix by a scalar or performs matrix multiplication.
    --
    -- @tparam Matrix self The matrix to multiply
    -- @tparam Matrix|number other The scalar or matrix to multiply with
    -- @treturn Matrix The resulting matrix
    --      Note: For matrix multiplication, the number of columns in self must equal the number of rows in other
    -- @usage m:mul(3)
    -- @usage m * 3
    -- @usage m1:mul(m2)
    -- @usage m1 * m2
    mul = function(self, other)
        expect(1, self, "number", "Matrix")
        expect(2, other, "number", "Matrix")

        if type(self) == "number" then
            return other * self
        end
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            if type(other) == "number" then
                for c = 1, self.columns do
                    m[r][c] = self[r][c] * other
                end
            elseif type(other) == "table" and self.rows == other.columns then
                for c = 1, other.columns do
                    for k = 1, self.columns do
                        m[r][c] = (m[r][c] or 0) + self[r][k] * other[k][c]
                    end
                end
            else
                error("Invalid Argument! Takes a scalar value or another matrix whose columns equal the first matrix's number of rows!")
            end
        end
        return from2DArray(m)
    end,

    --- Divides a matrix by a scalar or another matrix.
    --
    -- @tparam Matrix self The matrix to divide
    -- @tparam Matrix|number other The scalar or matrix to divide by
    -- @treturn Matrix The resulting matrix
    --      Note: Division by a matrix is performed by multiplying by its inverse
    -- @usage m:div(2)
    -- @usage m / 2
    -- @usage m1:div(m2)
    -- @usage m1 / m2
    div = function(self, other)
        expect(1, self, "number", "Matrix")
        expect(2, other, "number", "Matrix")

        if type(self) == "number" then
            return self * other:inverse()
        elseif type(other) == "number" then
            return self * (1 / other)
        elseif type(other) == "table" then
            return self * other:inverse()
        else
            error("Invalid Argument! Takes a scalar value or another square matrix!")
        end
    end,

    --- Negates all elements in a matrix.
    --
    -- @tparam Matrix self The matrix to negate
    -- @treturn Matrix The resulting negated matrix
    -- @usage m:unm()
    -- @usage -m
    unm = function(self)
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                m[r][c] = -self[r][c]
            end
        end
        return from2DArray(m)
    end,

    --- Raises a square matrix to a non-negative integer power.
    --
    -- @tparam Matrix self The matrix to raise to a power
    -- @tparam number n The non-negative integer power
    -- @treturn Matrix The resulting matrix
    --      Note: Raising to power 0 returns the identity matrix
    -- @usage m:pow(3)
    -- @usage m ^ 3
    pow = function(self, n)
        expect(1, n, "number")
        if self.rows ~= self.columns then
            error("Must be a square matrix to raise to a power!")
        end
        if type(n) ~= "number" or n < 0 or n ~= math.floor(n) then
            error("Power must be a non-negative integer!")
        end

        if n == 0 then
            local m = {}
            for r = 1, self.rows do
                m[r] = {}
                for c = 1, self.columns do
                    m[r][c] = (r == c) and 1 or 0
                end
            end
            return from2DArray(m)
        end

        local result = self
        for i = 2, n do
            result = result * self
        end
        return result
    end,

    --- Computes the total number of elements in the matrix.
    --
    -- @tparam Matrix self The matrix to measure
    -- @treturn number The total number of elements (rows * columns)
    -- @usage m:length()
    -- @usage #m
    length = function(self)
        return self.rows * self.columns
    end,

    --- Creates a string representation of the matrix.
    --
    -- @tparam Matrix self The matrix to stringify
    -- @treturn string The resulting string with each row on a new line
    -- @usage m:tostring()
    -- @usage m .. ""
    tostring = function(self)
        local s = ""
        for r = 1, self.rows do
            if #s > 0 then
                s = s .. "\n"
            end
            s = s .. "{ "
            for c = 1, self.columns do
                s = s .. tostring(self[r][c]) .. " "
            end
            s = s .. "}"
        end
        return s
    end,

    --- Determines if two matrices are equal.
    --
    -- @tparam Matrix self The first matrix to test
    -- @tparam Matrix other The other matrix to test against
    -- @treturn boolean True if the matrices have the same dimensions and all elements are equal
    -- @usage m1:equals(m2)
    -- @usage m1 == m2
    equals = function(self, other)
        expect(1, self, "Matrix")
        expect(2, other, "Matrix")

        if type(self) == type(other) and self.rows == other.rows and self.columns == other.columns then
            local identical = true
            for r = 1, self.rows do
                for c = 1, self.columns do
                    identical = self[r][c] == other[r][c]
                    if not identical then
                        return false
                    end
                end
            end
            return true
        end
        return false
    end,

    --- Computes the minor matrix by removing a specified row and column.
    --
    -- @tparam Matrix self The matrix to use
    -- @tparam number row The row index to remove
    -- @tparam number column The column index to remove
    -- @treturn Matrix The resulting minor matrix
    -- @usage m:minor(1, 2)
    minor = function(self, row, column)
        expect(1, row, "number")
        expect(2, column, "number")
        if row < 1 or row > self.rows or column < 1 or column > self.columns then
            error("Row and column indices must be within matrix bounds!")
        end

        local m = {}
        local minor_row = 1
        for r = 1, self.rows do
            if r ~= row then
                m[minor_row] = {}
                local minor_col = 1
                for c = 1, self.columns do
                    if c ~= column then
                        m[minor_row][minor_col] = self[r][c]
                        minor_col = minor_col + 1
                    end
                end
                minor_row = minor_row + 1
            end
        end
        return from2DArray(m)
    end,

    --- Computes the determinant of a square matrix.
    --
    -- @tparam Matrix self The matrix to use
    -- @treturn number The determinant value
    -- @usage m:determinant()
    determinant = function(self)
        if self.rows == 0 or self.columns == 0 then
            return 0
        end
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate the determinant!")
        end
        if self.rows == 2 then
            return self[1][1] * self[2][2] - self[1][2] * self[2][1]
        end

        local det = 0
        for c = 1, self.columns do
            local cofactor = ((-1) ^ (1 + c)) * self[1][c]
            det = det + cofactor * self:minor(1, c):determinant()
        end

        return det
    end,

    --- Computes the transpose of the matrix (rows become columns).
    --
    -- @tparam Matrix self The matrix to transpose
    -- @treturn Matrix The resulting transposed matrix
    -- @usage m:transpose()
    transpose = function(self)
        local m = {}
        for c = 1, self.columns do
            m[c] = {}
            for r = 1, self.rows do
                m[c][r] = self[r][c]
            end
        end
        return from2DArray(m)
    end,

    --- Computes the cofactor matrix.
    --
    -- @tparam Matrix self The matrix to use
    -- @treturn Matrix The resulting cofactor matrix
    -- @usage m:cofactor()
    cofactor = function(self)
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate cofactor matrix!")
        end

        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                local sign = ((-1) ^ (r + c))
                local minor_det = self:minor(r, c):determinant()
                m[r][c] = sign * minor_det
            end
        end

        return from2DArray(m)
    end,

    --- Computes the adjugate (adjoint) matrix.
    --
    -- @tparam Matrix self The matrix to use
    -- @treturn Matrix The resulting adjugate matrix
    -- @usage m:adjugate()
    adjugate = function(self)
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate adjugate!")
        end

        return self:cofactor():transpose()
    end,

    --- Computes the inverse of a square matrix.
    --
    -- @tparam Matrix self The matrix to invert
    -- @treturn Matrix The resulting inverse matrix
    -- @usage m:inverse()
    inverse = function(self)
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate inverse!")
        end

        local det = self:determinant()
        if det == 0 then
            error("Matrix is singular (determinant is zero) - no inverse exists!")
        end

        if self.rows == 1 then
            return from2DArray({{1 / self[1][1]}})
        end

        local adj = self:adjugate()
        return adj / det
    end,

    --- Computes the trace (sum of diagonal elements) of a square matrix.
    --
    -- @tparam Matrix self The matrix to use
    -- @treturn number The trace value
    -- @usage m:trace()
    trace = function(self)
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate trace!")
        end
        local sum = 0
        for i = 1, self.rows do
            sum = sum + self[i][i]
        end
        return sum
    end,

    --- Computes the rank of the matrix using row reduction.
    --
    -- @tparam Matrix self The matrix to use
    -- @treturn number The rank (number of linearly independent rows)
    -- @usage m:rank()
    rank = function(self)
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                m[r][c] = self[r][c]
            end
        end

        local rank = 0
        local row = 1

        for col = 1, self.columns do
            local pivot_row = nil
            for r = row, self.rows do
                if math.abs(m[r][col]) > 1e-10 then
                    pivot_row = r
                    break
                end
            end

            if pivot_row then
                m[row], m[pivot_row] = m[pivot_row], m[row]

                for r = row + 1, self.rows do
                    if math.abs(m[r][col]) > 1e-10 then
                        local factor = m[r][col] / m[row][col]
                        for c = col, self.columns do
                            m[r][c] = m[r][c] - factor * m[row][c]
                        end
                    end
                end

                rank = rank + 1
                row = row + 1

                if row > self.rows then
                    break
                end
            end
        end

        return rank
    end,

    --- Computes the Frobenius norm (square root of sum of squared elements).
    --
    -- @tparam Matrix self The matrix to measure
    -- @treturn number The Frobenius norm
    -- @usage m:frobenius_norm()
    frobenius_norm = function(self)
        local sum = 0
        for r = 1, self.rows do
            for c = 1, self.columns do
                sum = sum + self[r][c] * self[r][c]
            end
        end
        return math.sqrt(sum)
    end,

    --- Computes the max norm (maximum absolute value of any element).
    --
    -- @tparam Matrix self The matrix to measure
    -- @treturn number The max norm
    -- @usage m:max_norm()
    max_norm = function(self)
        local max_val = 0
        for r = 1, self.rows do
            for c = 1, self.columns do
                max_val = math.max(max_val, math.abs(self[r][c]))
            end
        end
        return max_val
    end,

    --- Computes the Hadamard product (element-wise multiplication).
    --
    -- @tparam Matrix self The first matrix
    -- @tparam Matrix other The second matrix (must have same dimensions)
    -- @treturn Matrix The resulting matrix
    -- @usage m1:hadamard_product(m2)
    hadamard_product = function(self, other)
        expect(1, self, "Matrix")
        expect(2, other, "Matrix")

        if self.rows ~= other.rows or self.columns ~= other.columns then
            error("Matrices must have same dimensions for element-wise multiplication!")
        end
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                m[r][c] = self[r][c] * other[r][c]
            end
        end
        return from2DArray(m)
    end,

    --- Computes element-wise division.
    --
    -- @tparam Matrix self The numerator matrix
    -- @tparam Matrix other The denominator matrix (must have same dimensions)
    -- @treturn Matrix The resulting matrix
    -- @usage m1:elementwise_div(m2)
    elementwise_div = function(self, other)
        expect(1, self, "Matrix")
        expect(2, other, "Matrix")

        if self.rows ~= other.rows or self.columns ~= other.columns then
            error("Matrices must have same dimensions for element-wise division!")
        end

        local m = {}
        for r = 1, other.rows do
            m[r] = {}
            for c = 1, other.columns do
                m[r][c] = 1 / other[r][c]
            end
        end

        return self:hadamard_product(from2DArray(m))
    end,

    --- Checks if the matrix is symmetric.
    --
    -- @tparam Matrix self The matrix to test
    -- @treturn boolean True if the matrix equals its transpose
    -- @usage m:is_symmetric()
    is_symmetric = function(self)
        if self.rows ~= self.columns then
            return false
        end
        for r = 1, self.rows do
            for c = r + 1, self.columns do
                if math.abs(self[r][c] - self[c][r]) > 1e-10 then
                    return false
                end
            end
        end
        return true
    end,

    --- Checks if the matrix is diagonal.
    --
    -- @tparam Matrix self The matrix to test
    -- @treturn boolean True if all off-diagonal elements are zero
    -- @usage m:is_diagonal()
    is_diagonal = function(self)
        if self.rows ~= self.columns then
            return false
        end
        for r = 1, self.rows do
            for c = 1, self.columns do
                if r ~= c and math.abs(self[r][c]) > 1e-10 then
                    return false
                end
            end
        end
        return true
    end,

    --- Checks if the matrix is an identity matrix.
    --
    -- @tparam Matrix self The matrix to test
    -- @treturn boolean True if the matrix is diagonal with all ones on the diagonal
    -- @usage m:is_identity()
    is_identity = function(self)
        if not self:is_diagonal() then
            return false
        end
        for i = 1, self.rows do
            if math.abs(self[i][i] - 1) > 1e-10 then
                return false
            end
        end
        return true
    end,

    --- Returns a copy of this matrix, with the same data.
    --
    -- @tparam Matrix self The matrix to copy
    -- @treturn Matrix A new matrix with the same data as the original
    -- @usage m2 = m1:clone()
    clone = function(self)
        return new(self.rows, self.columns, function(r, c) return self[r][c] end)
    end
}

local metatable = {
    __name = "Matrix",
    __index = matrix,
    __add = matrix.add,
    __sub = matrix.sub,
    __mul = matrix.mul,
    __div = matrix.div,
    __unm = matrix.unm,
    __pow = matrix.pow,
    __len = matrix.length,
    __tostring = matrix.tostring,
    __eq = matrix.equals
}