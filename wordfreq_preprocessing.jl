# a julia script to parse out of the word frequencies and agglomerate them
using DataFrames
using CSV
fname = "./data/data/Jun_data/augmented_English_4_word_info.csv"
df = CSV.read(fname)

print(typeof(df))

mutable struct results_row
    word_l::String
    wordfreq_l::Real
    norvig_wordfreq_l::Real
    word_information_l::Real
    bigram_frequencies_l::Real
    first_gaze_duration_l::Real
    #word_r::String
    #wordfreq_r::Real
    #norvig_wordfreq_r::Real
   # word_information_r::Real
    #bigram_frequencies_r::Real
    #first_gaze_duration_r::Real
end

function combine_leftright_rows(l, r)
    r = []
    for el in l
        push!(r, el)
    end
    for el in r
        push!(r, el)
    end
    return r
end

function collate_word_freqs()
    sumdurations_l = Dict{String, Real}()
    sumdurations_r = Dict{String, Real}()
    row_dict_l = Dict{String, Array{Any}}()
    row_dict_r = Dict{String, Array{Any}}()
    wordcounts_l = Dict{String, Real}()
    wordcounts_r = Dict{String, Real}()
    
    # define a generic missing row
    MissingRow = [missing, missing, missing, missing, missing, missing]


    res = DataFrame(word_l = String[],
                    wordfreq_l = Real[],
                    norvig_wordfreq_l = Real[],
                    word_information_l = Real[],
                    bigram_frequencies_l = Real[],
                    first_gaze_duration_l = Real[],
                    word_r = String[],
                    wordfreq_r = Real[],
                    norvig_wordfreq_r = Real[],
                    word_information_r = Real[],
                    bigram_frequencies_r = Real[],
                    first_gaze_duration_r = Real[],

                    )
    
    print("Before dataframe definitions!")
    res_l = DataFrame(word_l = String[],
                    wordfreq_l = Real[],
                    norvig_wordfreq_l = Real[],
                    log_norvig_l = Real[],
                    word_information_l = Real[],
                    word_information_gain_l = Real[],
                    bigram_frequencies_l = Real[],
                    first_gaze_duration_l = Real[],
                    total_gaze_duration_l = Real[],
                    average_gaze_duration_l = Real[],
                    )
    
    res_r = DataFrame(
                    word_r = String[],
                    wordfreq_r = Real[],
                    norvig_wordfreq_r = Real[],
                    log_norvig_r = Real[],
                    word_information_r = Real[],
                    word_information_gain_r = Real[],
                    bigram_frequencies_r = Real[],
                    first_gaze_duration_r = Real[],
                    total_gaze_duration_r = Real[],
                    average_gaze_duration_r = Real[],
                    )
    
    
    # allowing missing!
    allowmissing!(res_l, :bigram_frequencies_l)
    allowmissing!(res_r,:bigram_frequencies_r)
    allowmissing!(res_l, :norvig_wordfreq_l)
    allowmissing!(res_r,:norvig_wordfreq_r)
    allowmissing!(res_l, :log_norvig_l)
    allowmissing!(res_r,:log_norvig_r)
    print("Starting row iterations")
    for row in eachrow(df)
        word_l = row[:word_l]
        word_r = row[:word_r]
        if (haskey(sumdurations_l, word_l))
            sumdurations_l[word_l] += row[:thisSaccDuration_l]
            wordcounts_l[word_l] +=1
        end
        if(haskey(sumdurations_r, word_r))
            sumdurations_r[word_r] +=row[:thisSaccDuration_r]
            wordcounts_r[word_r] +=1
        end
        if !(haskey(sumdurations_l, word_l))
            r = [
                            word_l,
                            row[:wordFreq_l],
                            row[:norvig_wordFreq_l],
                            row[:log_norvig_wordFreq_l],
                            row[:word_information_l],
                            row[:word_information_gain_l],
                            row[:bigram_frequencies_l],
                            row[:thisSaccDuration_l],
                            #word_r,
                            #row[:wordFreq_r],
                            #row[:norvig_wordFreq_r],
                            #row[:word_information_r],
                            #row[:bigram_frequencies_r],
                            #row[:thisSaccadeDuration_r],
            ]
            row_dict_l[word_l] = r
            wordcounts_l[word_l] = 1
            sumdurations_l[word_l] = row[:thisSaccDuration_l]
        end
        if !(haskey(sumdurations_r, word_r))
            r = [
                            word_r,
                            row[:wordFreq_r],
                            row[:norvig_wordFreq_r],
                            row[:log_norvig_wordFreq_r],
                            row[:word_information_r],
                            row[:word_information_gain_r],
                            row[:bigram_frequencies_r],
                            row[:thisSaccDuration_r],
            ]
            row_dict_r[word_r] = r
            wordcounts_r[word_r] = 1
            sumdurations_r[word_r] = row[:thisSaccDuration_r]
        end
    end

    for (k,v) in row_dict_l
        # append the sum to the row
        push!(v,sumdurations_l[k])

        push!(v, sumdurations_l[k]/wordcounts_l[k])
        # and add to datafrme
        push!(res_l, v)
    end
    print("Added left rows")
    for (k,v) in row_dict_r
        push!(v, sumdurations_r[k])

        push!(v, sumdurations_r[k] / wordcounts_r[k])

        push!(res_r, v)
        # add the average count    end
    end
    print("added right rows")
    # then save the two
    print("Saving resulting data frames")
    out_base = "data/data/Jun_data/English_wordfreq"
    CSV.write(out_base * "_l.csv", res_l)
    CSV.write(out_base * "_r.csv", res_r)
    print("All done!")
    
end
    

collate_word_freqs()