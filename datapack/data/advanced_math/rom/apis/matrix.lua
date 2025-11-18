local matrix = {
    add = function(self, other)
        if type(self) == "number" then
            return other + self
        end
        local m = {}
        for r = 1, self.rows do
            m[r] = {}
            for c = 1, self.columns do
                if type(other) == "number" then
                    m[r][c] = self[r][c] + other
                elseif type(other) == "table" and getmetatable(other).__index == getmetatable(self).__index then
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

    sub = function(self, other)
        return self + (-other)
    end,

    mul = function(self, other)
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
            elseif type(other) == "table" and getmetatable(other).__index == getmetatable(self).__index and self.rows == other.columns then
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

    div = function(self, other)
        if type(self) == "number" then
            return self * other:inverse()
        elseif type(other) == "number" then
            return self * (1 / other)
        elseif type(other) == "table" and getmetatable(other).__index == getmetatable(self).__index then
            return self * other:inverse()
        else
            error("Invalid Argument! Takes a scalar value or another square matrix!")
        end
    end,

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

    pow = function(self, n)
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

    length = function(self)
        return self.rows * self.columns
    end,

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

    equals = function(self, other)
        if type(self) == type(other) and type(self) == "table" and getmetatable(other).__index == getmetatable(self).__index and self.rows == other.rows and self.columns == other.columns then
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

    minor = function(self, row, column)
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

    adjugate = function(self)
        if self.rows ~= self.columns then
            error("Must be a square matrix to calculate adjugate!")
        end

        return self:cofactor():transpose()
    end,

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

    frobenius_norm = function(self)
        local sum = 0
        for r = 1, self.rows do
            for c = 1, self.columns do
                sum = sum + self[r][c] * self[r][c]
            end
        end
        return math.sqrt(sum)
    end,

    max_norm = function(self)
        local max_val = 0
        for r = 1, self.rows do
            for c = 1, self.columns do
                max_val = math.max(max_val, math.abs(self[r][c]))
            end
        end
        return max_val
    end,

    hadamard_product = function(self, other)
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

    elementwise_div = function(self, other)
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

    clone = function(self)
        return new(self.rows, self.columns, function(r, c) return self[r][c] end)
    end
}

local metatable = {
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

function new(rows, columns, func)
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

function from2DArray(arr)
    return new(#arr, #arr[1], function(r, c) return arr[r][c] or 0 end)
end

function fromVector(v, row)
    row = row or true
    if getmetatable(v).__index ~= getmetatable(vector.new()).__index then
        error("Invalid Argument! Takes a vector!")
    end
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

function identity(rows, columns)
    return new(rows, columns)
end