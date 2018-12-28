# file: format-report.rb

flines = File.open(ARGV[0]).readlines

column_map = { 
  "from_name1"  => "to_name1", 
  "from_name2"  => "to_name2",  
}

File.open( "work.csv","w+") do |fl|  
  fl.puts "header1,header2,header3,header4"
  flines.each do |l|
    a = l.split(",")

    # Time, mapping-defined-in-column_map, original-column-2
    fl.puts Time.now.strftime("%m/%d/%Y") + "," + 
            (column_map[a[1]] || a[1]) + "," + a[2] "," + a[3]
  end
end
