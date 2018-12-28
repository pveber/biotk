(* module No_control : sig
 *   module Peak : sig
 *     type tabular t = {
 *       chrom : string ;
 *       chromStart : int ;
 *       chromEnd : int ;
 *       len "length" : int ;
 *       summit : int ;
 *       tags : int ;
 *       pvalue : float ;
 *       fold : float ;
 *     }
 *   end
 * 
 *   val read_xls : in_channel -> Peak.t Stream.t
 * end
 * 
 * module With_control : sig
 *   module Peak : sig
 *     type tabular t = {
 *       chrom : string ;
 *       chromStart : int ;
 *       chromEnd : int ;
 *       len "length" : int ;
 *       summit : int ;
 *       tags : int ;
 *       pvalue : float ;
 *       fold : float ;
 *       fdr : float ;
 *     }
 *   end
 * 
 *   val read_xls : in_channel -> Peak.t Stream.t
 * end *)
