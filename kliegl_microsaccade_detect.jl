#Julia implementation of the kliegl microsaccade detection algorithm on the eyetracking dataset
pwd()

using CSV
using DataFrames
using Query


fname = "Parser_test_Saccades.csv"
df = CSV.read(fname, delim=",")

"""
q = @from i in df begin
    @select i.Eye
    @collect DataFrame
end

print(typeof(q))
print(head(q))
"""
#names(df)
#print(names(df))
#print(df.Eye)
# aha! so it's very simple

# let's addthis function to df

function names_to_string_array(arr::AbstractArray{Symbol})
    s = []
    for sym in arr
        push!(s, string(sym))
    end
    return s
end

function findfirstindex(arr::Array{Symbol, 1}, s::String)
    l = length(arr)
   # print(l)
    for i in 1:l
        #print(i)
        #print(l)
        #print("\n")
        elem = arr[i]
        if s == string(elem)
            return i
        end
    end
    return 0
end
    

function getColumn(df::DataFrame, s::String)
    index = findfirstindex(names(df), s)
   # print("\n")
    ##print("in blib!")
    #print(index)
    #print("\n")
    if index == 0
        return Nothing
    end
    #print(index)
    return df[index]
end

function getColumns(df::DataFrame, arr::Array{String})
    indices = []
    namelist = names(df)
    for s in arr
        i = findfirstindex(namelist, s)
        push!(indices, i)
    end
    return df[indices]
end
function get_velocities(df::DataFrame)
    lxpositions = getColumn(df, "xPositionLeft")
    rxpositions = getColumn(df, "xPositionRight")
    lypositions = getColumn(df, "yPositionLeft")
    rypositions = getColumn(df, "yPositionRight")
    times = getColumn(df, "timeStamp")

    @assert length(lxpositions) == length(rxpositions) == length(lypositions) == length(rypositions) == length(times)
    lvx = get_velocity(lxpositions, times)
    rvx = get_velocity(rxpositions, times)
    lvy = get_velocity(lypositions,times)
    rvy = get_velocity(rypositions, times)
    
    # now add them into the data frame
    df.LeftXVelocity = lvx
    df.RightXVelocity = rvx
    df.LeftYVelocity = lvy
    df.RightYVelocity = rvy
    return df
    
end

function get_velocity(arr::Array{Union{Missing, AbstractFloat}}, times::Array{Union{Missing, Int}})
    l = length(arr)
    vs = []
    # add zeroth velocity
    v0 = arr[2] - arr[1] / times[2] - times[1]
    push!(vs, v0)
    for i in 2:(l - 1)
        v = arr[i+1] - arr[i-1] / (times[i+1] - times[i-1])
        push!(vs, v)
    end
    vend = arr[l] - arr[l-1] / times[l] - times[l-1]
    push!(vs, vend)
    return vs
end

function median_variance(arr::Array{Real,1})
    return median(arr**2) - median(arr) **2
end

function kliegl_microsaccade_detect(df::DataFrame, threshold::Real)
    #create velocity columns
    df = get_velocities(df)
    mvarlx = median_variance(df.LeftXVelocity)
    mvarly = median_variance(df.LeftYVelocity)
    mvarrx = median_variance(df.RightXVelocity)
    mvarry = median_varaince(df.RightYVelocity)
    
    radiuslx = mvarlx * threshold
    radiusly = mvarly * threshold
    radiusrx = mvarrx * threshold
    radiusry = mvarry * threshold
    df.LeftSaccade = map((x,y) -> (x/radiuslx)**2 + (y/radiusly)**2 > 1, df.LeftVelocityX, df.LeftVelocityY)
    df.RightSaccade = map((x,y)-> (x/radiusrx)**2 + (y/radiusry)**2 > 1, df.RightVelocityX, df.RightVelocityY)
    return df
end
