#Simple function to merge parsed files from the ASC parser.
include("ASC_parser.jl")


function merge_files(infile::AbstractString, outfile::AbstractString, combine_eyes=false, merge_types=false, delim::AbstractString = ",")
    for (root, dirs, files) in walkdir(infile)
        num = 0
        for file in files
            fname, last = split(string(file), ".")
            if last == "asc"
                blocklist = parse_asc_file(joinpath(root, file))
                if merge_eyes == true
                    blocklist = merge_eyes(blockList)
                end
                if merge_types== true
                    if num == 0
                        write_merged_csv(outfile, blocklist,delim, fname,true)
                    else
                        write_merged_csv(outfile, blocklist, delim, fname, false)
                    end
                else
                    if num == 0
                        write_type_split_csv(outfile, blocklist, delim, fname, true)
                    else
                        write_type_split_csv(outfile, blocklist, delim, fname, false)
                    end
                end
            end
        end
    end    
end

merge_files("English", "Results/English")