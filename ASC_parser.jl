# This script contains a parser for ASC files produced by the Eyelink Eyetracker.

#Declare structs
abstract type EyelinkType end
abstract type EyeEvent <: EyelinkType end

mutable struct EyelinkMessage <: EyelinkType
    time::Int
    message::AbstractString
end

mutable struct EyelinkButton <: EyelinkType
    time::Int
    button_number::Int
    state::AbstractString
end

mutable struct Block <: EyelinkType
    eye::AbstractString
    startTime::Int
    endTime::Int
    event_type::AbstractString
    xresolution::AbstractFloat
    yresolution::AbstractFloat
    completed::Bool
    events::Array{EyeEvent}

end

mutable struct Fixation <: EyeEvent
    eye::AbstractString
    startTime::Int
    endTime::Int
    duration::AbstractFloat
    average_x::AbstractFloat
    average_y::AbstractFloat
    average_pupil::AbstractFloat
    xresolution::AbstractFloat
    yresolution::AbstractFloat
    completed::Bool
end

mutable struct Saccade <: EyeEvent
    eye::AbstractString
    startTime::Int
    endTime::Int
    duration::AbstractFloat
    startX::AbstractFloat
    startY::AbstractFloat
    endX::AbstractFloat
    endY::AbstractFloat
    amplitude::AbstractFloat
    peakVelocity::AbstractFloat
    xresolution::AbstractFloat
    yresolution::AbstractFloat
    completed::Bool
end

mutable struct Blink <: EyeEvent
    eye::AbstractString
    startTime::Int
    endTime::Int
    duration::AbstractFloat
    completed::Bool
end

mutable struct EyelinkSample <: EyeEvent
    timeStamp::Int
    xPositionLeft::AbstractFloat
    yPositionLeft::AbstractFloat
    pupilSizeLeft::AbstractFloat
    xPositionRight::AbstractFloat
    yPositionRight::AbstractFloat
    pupilSizeRight::AbstractFloat
    velocityLeft::AbstractFloat
    velocityRight::AbstractFloat
    xResolution::AbstractFloat
    yResolution::AbstractFloat
end

#test filename
filename = "SUB80B1D.asc"

filename = "English/Block1/ASC/SUB01B1E.asc"

const emptyBlock = Block("",0,0,"",0,0,true,[])
const emptyLeftFixation = Fixation("L", -1,-1,-1,-1,-1,-1,-1,-1,true)
const emptyRightFixation = Fixation("R",-1,-1,-1,-1,-1,-1,-1,-1,true)
const emptyLeftSaccade = Saccade("L", -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,true)
const emptyRightSaccade = Saccade("R", -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,true)
const emptyLeftBlink = Blink("L", -1,-1,-1, true)
const emptyRightBlink = Blink("R", -1,-1,-1,true)


const keywordList = ["START", "END", "EFIX", "SFIX", "SSACC", "ESACC","SBLINK", "EBLINK", "MSG"]

function isSkippable(startElem::AbstractString)
    if startElem == "" ||
        startElem == "**" ||
        startElem == ">>>>>>>" ||
        startElem == "MSG"
        return true
    end
    return false
end

function concatSubstrings(l::Array{SubString})
    s = ""
    for e in l
        s * " " * e
    end
    return s
end

function default_parse(t::Type,l::Any, default::Any)
    res = tryparse(t, l)
    if typeof(res) == Nothing
        # i.e. if parsing failed
        print("Parse failed on $l")
        return default
    end
    return res
end

function create_open_file(fname, firstf=false)
    if !isfile(fname)
        touch(fname)
    end
    if firstf == true
        f = open(fname, "w+")
    else
        #print("writing in append mode!")
        f = open(fname, "a")
    end
    return f
end

function getfields(obj)
    t = typeof(obj)
    fields = fieldnames(t)
    names = [getfield(obj, field) for field in fields]
    return names
end

    

function parse_sample(elems)
    time = default_parse(elems[1], Int, -1)
    xpl = default_parse(elems[2], Float64, -1)
    ypl = default_parse(elems[3], Float64, -1)
    psl = default_parse(elems[4], Float64, -1)
    xpr = default_parse(elems[5], Float64, -1)
    ypr = default_parse(elems[6], Float64, -1)
    psr = default_parse(elems[7], Float64, -1)
    xvl = default_parse(elems[8], Float64, -1)
    yvl = default_parse(elems[9], Float64, -1)
    xvr = default_parse(elems[10], Float64, -1)
    yvr = default_parse(elems[11], Float64, -1)
    xr = default_parse(elems[12], Float64, -1)
    yr = default_parse(elems[13], Float64, -1)
    return EyelinkSample(time, xpl, ypl, psl, xpr, ypr, psr, xvl, yvl, xvr, yvr, xr, yr)
end

function stringify_array(arr::Array{Any})
   return [string(a) for a in arr] 
end

function write_csv_line(f::IOStream, elems, delim::AbstractString)
    
    s = create_csv_line(elems, delim)
    write(f, s)
end

function create_csv_line(elems, delim::AbstractString)
    s = ""
    for elem in elems
        if elem == -1
            s = s * delim # write nothing on the default -1s - i.e. the missing values!
        else
            s = s *string(elem) * delim
        end
    end
    s = chop(s) # this is because it adds an extra delim at the end to take off
    s = s * "\n"
    s
end

function write_type_split_csv(basename::AbstractString, blockList::Array{Block}, delim::AbstractString, subject::Union{AbstractString, Bool}, write_first=true)
    fixColnames = ["Block","Eye", "StartTime", "EndTime", "Duration", "AverageX","AverageY", "AveragePupil", "XResolution","YResolution", "Completed"]
    saccColnames = ["Block","Eye", "StartTime","EndTime","Duration","StartX","StartY","EndX","EndY","Amplitude","PeakVelocity","XResolution","YResolution","Completed"]
    blinkColnames = ["Block","Eye","StartTime","EndTime", "Duration", "Completed"]
    
    if subject != false
        #prepend!(fixColnames, "Subject")
        #prepend!(saccColnames, "Subject")
        #prepend!(blinkColnames, "Subject")
        fixColnames = ["Subject", fixColnames...]
        saccColnames = ["Subject", saccColnames...]
        blinkColnames = ["Subject", blinkColnames...]
    end
    
    fixname = basename * "_Fixations.csv"
    saccname = basename * "_Saccades.csv"
    blinkname = basename * "_Blinks.csv"
    # I hope having three open file descriptors isn't going to be too bad on memory!
    fixf = create_open_file(fixname, write_first)
    saccf = create_open_file(saccname, write_first)
    blinkf = create_open_file(blinkname, write_first)
    
    #write the colum headers
    if write_first == true
        write_csv_line(fixf, fixColnames, delim)
        write_csv_line(saccf, saccColnames, delim)
        write_csv_line(blinkf, blinkColnames,delim)
    end
    
    blocknumber = 0
    for block in blockList
        for evt in block.events
            s = ""
            if subject != false
                s = s * subject * delim
            end
            s = s * string(blocknumber) * delim
            if typeof(evt) == typeof(emptyLeftFixation)
                s = s * create_csv_line(getfields(evt), delim)
                write(fixf, s)
            end
            if typeof(evt) == typeof(emptyLeftSaccade)
               s = s * create_csv_line(getfields(evt), delim)
               write(saccf, s)
            end
            if typeof(evt) == typeof(emptyLeftBlink)
                s = s * create_csv_line(getfields(evt), delim)
                write(blinkf, s)
            end
        end
        blocknumber +=1
    end 
        
    close(fixf)
    close(saccf)
    close(blinkf)
end


function create_merged_csv_line(evt::EyeEvent, fnames::Vector{Symbol}, delim::AbstractString)
    t = typeof(evt)
    fields = fieldnames(t)
    s = string(t) * delim # add the type information right at the start of each line!
    for i in 1:length(fnames)
        field = fnames[i]
        if field in fields
            val = getfield(evt, field)
            if val != -1
                s = s * string(val) * delim
            else
                s = s * delim
            end
        end
        if !(field in fields)
            s = s * delim
        end
    end
    s = chop(s)
    s = s * "\n"
    s
end


function write_merged_csv_line(f::IOStream, evt::EyeEvent, fnames::Vector{Symbol}, delim::AbstractString)
    s = create_merged_csv_line(evt, fnames, delim)
    write(f, s)
end

function write_merged_csv(basename::AbstractString, blockList::Array{Block}, delim::AbstractString, subject::Union{AbstractString, Bool}, write_first=true)
    if !endswith(basename, ".csv")
        basename  = basename * ".csv"
    end
    fi= create_open_file(basename, write_first)
    fieldnames = [:eye, :startTime, :endTime, :duration, :startX, :startY, :endX, :endY, :average_x, :average_y, :average_pupil, :amplitude, :peakVelocity, :xresolution, :yresolution, :completed]
    colnames = ["Block", "Type", "Eye", "StartTime", "EndTime", "Duration","StartX", "StartY","EndX","EndY","AverageX","AverageY","AveragePupil","Amplitude","PeakVelocity","XResolution", "YResolution","Completed"]
    if subject != false
        colnames = ["Subject", colnames...]
    end
    if write_first == true
        write_csv_line(fi, colnames, delim)
    end
    blocknumber= 0
    for block in blockList
        for evt in block.events
            s = ""
            if subject !=false
                s *= subject * delim
            end
            s = s * string(blocknumber) * delim
            s = s * create_merged_csv_line(evt, fieldnames, delim)
            write(fi, s)
        end
        blocknumber +=1
    end 

end

function write_messages_csv(basename::AbstractString, messageList::Array{EyelinkMessage}, delim::AbstractString)
    if !endswith(basename, ".csv")
        basename  = basename * ".csv"
    end
    fi= create_open_file(basename)
    colnames= ["Type", "Message"]
    write_csv_line(fi, colnames, delim)
    for message in messageList
        write_csv_line(fi, getfields(message),delim)
    end
    close(fi)
end
    
function get_column_index(arr::Array{AbstractString},key::AbstractString)
    return findfirst(arr, key)
end

function read_csv_line(line::AbstractString, sep::AbstractString)
    arr = split(strip(line), sep)
    return arr
end


function merge_eyes(eye1::EyeEvent, eye2::EyeEvent)
    #TODO
    return 1
end


function basic_eye_matching(blockList::Array{Block})
    currentLeft = 0
    currentRight = 0
    for i in 1:length(blockList)
        block = blockList[i]
        eventList::Array{EyeEvent}
        for evt in block.events
            eye = evt.eye
            if eye == "L"
                if currentRight != 0
                    if currentLeft != 0
                        push!(eventList, currentLeft) # assume current left is now forever stuck behind if no right
                        currentLeft = 0
                    end
                    merged = merge_eyes(evt, currentRight)
                    currentRight = 0
                    push!(eventList, merged)
                end
                if currentRight == 0
                    if currentLeft == 0
                        currentLeft = evt
                    else
                        push!(eventList, currentLeft) # assume current left is left behind
                        currentLeft = 0
                    end
                end
            end
            if eye == "R"
                if currentLeft != 0
                    if currentRight != 0
                        push!(eventList, currentRight) # assume current left is now forever stuck behind if no right
                        currentRight = 0
                    end
                    merged = merge_eyes(evt, currentLeft)
                    currentLeft == 0
                    push!(eventList, merged)
                end
                if currentLeft == 0
                    if currentRight == 0
                        currentRight = evt
                    else
                        push!(eventList, currentRight) # assume current left is left behind
                        currentRight = 0
                    end
                end
            
            end
        end
        blockList[i].events = eventList
    end
    return blockList
end

function parse_asc_file(filename::AbstractString, save_messages=false)
    
   # if subject_in_filename == true
   #         splits = split(filename, "/")[end]
   #         subject = split(splits, ".")[1]
   # end
    
    currentBlock::Block = emptyBlock
    blockList::Array{Block} = []
    messageList::Array{EyelinkMessage} = []
    currentLeftFixation::Fixation = emptyLeftFixation
    currentRightFixation::Fixation = emptyRightFixation
    currentLeftSaccade::Saccade = emptyLeftSaccade
    currentRightSaccade::Saccade = emptyRightSaccade
    currentLeftBlink::Blink = emptyLeftBlink
    currentRightBlink::Blink = emptyRightBlink
    
    function resetCurrentEvents()
        currentLeftFixation = emptyLeftFixation
        currentRightFixation= emptyRightFixation
        currentLeftSaccade = emptyLeftSaccade
        currentRightSaccade = emptyRightSaccade
        currentLeftBlink = emptyLeftBlink
        currentRightBlink = emptyRightBlink
    end
    
    
    open(filename) do f
        val = 0
        N = 20000
        for line in eachline(f)
            val +=1
           #print("Parsing line $val")
            elems=  split(strip(line), r" |\t", keepempty=false)
            if length(elems) >=1
                firstElem = elems[1]
                if isSkippable(firstElem)
                    # skips lines that you're not interested in
                    #print("Skipping: " * firstElem * "\n")
                    continue
                end
                if !(firstElem in keywordList)
                    #print("Does not contain: " * firstElem * "\n")
                    continue
                end
               # print("Successfully grabbed: " * firstElem * "\n")
               # print(elems)
               # print("\n")
                if firstElem == "START"
                    # check current block is not closed
                    if currentBlock.completed == false
                        print(currentBlock.completed)
                        print(currentBlock)
                        throw("Previous block not completed!!!")
                    end
                    
                    # reset the events counter
                    resetCurrentEvents()
                    
                    time::Int = parse(Int, elems[2])
                    eye::AbstractString = convert(String, elems[3])
                    Type::AbstractString = convert(String, elems[4])
                    newBlock = Block(eye, time, -1, Type, -1, -1, false, [])
                    currentBlock = newBlock
                    push!(blockList, newBlock)
                end
                if firstElem == "END"
                    if currentBlock.completed == true
                        throw("Ending an already completed block!")
                    end
                    time = default_parse(Int, elems[2], -1)
                    Type= convert(String, elems[3])
                    currentBlock.endTime = time
                    currentBlock.completed=true
                    l = length(blockList)
                    #print("Parsed block $l \n")
                    #print(currentBlock)
                    #print("\n")
                end
                if firstElem == "SFIX"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftFixation.completed == false
                            throw("Left Starting an already in progress fixation")
                        end
                        newFix = Fixation(eye, default_parse(Int, elems[3],-1), -1, -1,-1,-1,-1,-1,-1,false)
                        currentLeftFixation = newFix
                        push!(currentBlock.events, newFix)
                    end
                    if eye == "R"
                        if currentRightFixation.completed == false
                            print("\n")
                            print(currentRightFixation)
                            throw("Right Starting an already in progress fixation")
                        end
                        newFix = Fixation(eye, default_parse(Int, elems[3],-1), -1, -1,-1,-1,-1,-1,-1,false)
                        currentRightFixation = newFix
                        push!(currentBlock.events, newFix)
                    end
                end
                if firstElem == "EFIX"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftFixation.completed ==true
                            throw(" Left Ending already completed fixation!")
                        end
                        currentLeftFixation.eye = eye
                        stime = parse(Int, elems[3])
                        if stime != currentLeftFixation.startTime
                            throw("Left Fixation start times do not match!")
                        end
                        currentLeftFixation.startTime = stime
                        currentLeftFixation.endTime = default_parse(Int, elems[4],-1)
                        currentLeftFixation.duration = default_parse(Float64, elems[5],-1)
                        currentLeftFixation.average_x = default_parse(Float64, elems[6],-1)
                        currentLeftFixation.average_y = default_parse(Float64, elems[7],-1)
                        currentLeftFixation.average_pupil = default_parse(Float64, elems[8],-1)
                        currentLeftFixation.completed = true
                    end
                    if eye == "R"
                        if currentRightFixation.completed==true
                            throw("Right Ending already completed fixation!")
                        end
                        currentRightFixation.eye = eye
                        stime = parse(Int, elems[3])
                        if stime != currentRightFixation.startTime
                            throw("Right Fixation start times do not match!")
                        end
                        currentRightFixation.startTime = stime
                        currentRightFixation.endTime = default_parse(Int, elems[4],-1)
                        currentRightFixation.duration = default_parse(Float64, elems[5],-1)
                        currentRightFixation.average_x = default_parse(Float64, elems[6],-1)
                        currentRightFixation.average_y = default_parse(Float64, elems[7],-1)
                        currentRightFixation.average_pupil = default_parse(Float64, elems[8],-1)
                        currentRightFixation.completed = true
                    end
                end
                if firstElem == "SSACC"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftSaccade.completed == false
                            throw("Left Starting with an already in progress saccade!")
                        end
                        stime = parse(Int, elems[3])
                        newSaccade = Saccade(String(eye), stime, -1, -1, -1,-1,-1,-1,-1,-1,-1,-1,false)
                        currentLeftSaccade = newSaccade
                        push!(currentBlock.events,newSaccade)
                    end
                    if eye == "R"
                        if currentRightSaccade.completed == false
                            throw("Left Starting with an already in progress saccade!")
                        end
                        stime = parse(Int, elems[3])
                        newSaccade = Saccade(String(eye), stime, -1, -1, -1,-1,-1,-1,-1,-1,-1,-1,false)
                        currentRightSaccade = newSaccade
                        push!(currentBlock.events,newSaccade)
                    end
                        
                end
                if firstElem == "ESACC"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftSaccade.completed == true
                            throw("Left Ending an already completed saccade!")
                        end
                        stime = parse(Int, elems[3])
                        if currentLeftSaccade.startTime != stime
                            throw("Left Saccade start times do not match!")
                        end
                        try
                            currentLeftSaccade.endTime = default_parse(Int, elems[4],-1)
                            currentLeftSaccade.duration = default_parse(Float64, elems[5],-1)
                            currentLeftSaccade.startX = default_parse(Float64, elems[6],-1)
                            currentLeftSaccade.startY = default_parse(Float64, elems[7],-1)
                            currentLeftSaccade.endX = default_parse(Float64, elems[8],-1)
                            currentLeftSaccade.endY = default_parse(Float64, elems[9],-1)
                            currentLeftSaccade.amplitude = default_parse(Float64, elems[10],-1)
                            currentLeftSaccade.peakVelocity = default_parse(Float64, elems[11],-1)
                            currentLeftSaccade.completed= true
                        catch
                            print("\n")
                            print("In catch statement!")
                            print(elems)
                            throw("Error in parsing")
                        end
                        
                    end
                    if eye == "R"
                        if currentRightSaccade.completed == true
                            throw("Right Ending an already completed saccade!")
                        end
                        stime = parse(Int, elems[3])
                        if currentRightSaccade.startTime != stime
                            throw("Right Saccade start times do not match!")
                        end
                        currentRightSaccade.endTime = default_parse(Int, elems[4],-1)
                        currentRightSaccade.duration = default_parse(Float64, elems[5],-1)
                        currentRightSaccade.startX = default_parse(Float64, elems[6],-1)
                        currentRightSaccade.startY = default_parse(Float64, elems[7],-1)
                        currentRightSaccade.endX = default_parse(Float64, elems[8],-1)
                        currentRightSaccade.endY = default_parse(Float64, elems[9],-1)
                        currentRightSaccade.amplitude = default_parse(Float64, elems[10],-1)
                        currentRightSaccade.peakVelocity = default_parse(Float64, elems[11],-1)
                        currentRightSaccade.completed= true
                    end
                end
                if firstElem == "SBLINK"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftBlink.completed == false
                            throw("Left Startin a blink with one already in progress")
                        end
                        stime = default_parse(Int, elems[3],-1)
                        newBlink = Blink(String(eye), stime, -1, -1, false)
                        currentLeftBlink = newBlink
                        push!(currentBlock.events,newBlink)
                    end
                    if eye == "R"
                        if currentRightBlink.completed == false
                            throw("Right Startin a blink with one already in progress")
                        end
                        stime = default_parse(Int, elems[3],-1)
                        newBlink = Blink(String(eye), stime, -1, -1, false)
                        currentRightBlink = newBlink
                        push!(currentBlock.events,newBlink)
                    end
                end
                if firstElem == "EBLINK"
                    eye = elems[2]
                    if eye == "L"
                        if currentLeftBlink.completed == true
                            throw("Left Ending with an already completed blink!")
                        end
                        stime = parse(Int, elems[3])
                        if currentLeftBlink.startTime != stime
                            throw("Left Start times for blink do not match!")
                        end
                        currentLeftBlink.endTime = default_parse(Int, elems[4],-1)
                        currentLeftBlink.duration = default_parse(Float64, elems[5],-1)
                        currentLeftBlink.completed = true
                    end
                    if eye == "R"
                        if currentRightBlink.completed == true
                            throw("Left Ending with an already completed blink!")
                        end
                        stime = parse(Int, elems[3])
                        if currentRightBlink.startTime != stime
                            throw("Right Start times for blink do not match!")
                        end
                        currentRightBlink.endTime = default_parse(Int, elems[4],-1)
                        currentRightBlink.duration = default_parse(Float64, elems[5],-1)
                        currentRightBlink.completed = true
                    end
                end
                if save_messages == true
                    if firstElem == "MSG"
                        time = parse(Int,elems[2])
                        message_string = concatSubstrings(elems[3:length(elems)])
                        message = Message(time, message_string)
                        push!(messageList, message)
                    end
                end
                
                if typeof(tryparse(Int, firstElem)) != Nothing
                        print("SAMPLE PARSED! $val \n")
                        print(elems)
                        print(firstElem)
                        print(typeof(tryparse(Int, firstElem)))
                        sample = parse_sample(elems)
                        push!(blockList.events, sample)
                end
                    
            end

        end
    end
    #print("Parsed File!")
   # #write_merged_csv("Parser_test", blockList, ",")
   # write_type_split_csv("Parser_test",blockList, ",", subject)
   print("Parsed file $filename \n")
   return blockList
end
            