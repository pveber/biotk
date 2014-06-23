module No_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      len "length" : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
    }
  end

  let read_xls ic = Peak.Row.stream_of_channel ~header:true ic
end

module With_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      len "length" : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
      fdr : float ;
    }
  end

  let read_xls ic = Peak.Row.stream_of_channel ~header:true ic
end
