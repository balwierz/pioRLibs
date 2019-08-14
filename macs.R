import.narrowPeakMACS = function(file, genome=NULL)
{
  require(GenomicRanges)
  ret = read.table(file, header=F, sep="\t", stringsAsFactors=F)
  colnames(ret) = c("seqnames", "start", "end", "id", "score", "strand", "FC", "minLog10pVal", "minLog10qVal", "summitPos")
  ret$start = ret$start + 1; # UCSC to GRanges
  # ret$summitPos = ret$start + ret$summitPos  # not sure about off-by-one
  ret = GRanges(ret)
  if(! is.null(genome))
  {
    GenomeInfoDb::seqlevels(ret) = GenomeInfoDb::seqlevels(genome)
    GenomeInfoDb::seqlengths(ret) = GenomeInfoDb::seqlengths(genome)
    GenomeInfoDb::genome(ret) = GenomeInfoDb::genome(genome)
  }
  return(ret)
}



import.narrowPeakMACS.purrr = function(files, seqinfo=NULL)
{
	require(purrr)
	files %>%
		`names<-`(., make.names(unlist(lapply(lapply(strsplit(basename(.), split="\\."), `[`, c(2,3)), paste, collapse=" ")))) %>%
		map(read.table, stringsAsFactors=F) %>%
		map(~GRanges(.$V1, IRanges(.$V2+1, .$V3), score=.$V5, fc=.$V7, mLog10pval=.$V8, mLog10qval=.$V9, summitPos=.$V2+.$V10+1, seqinfo=seqinfo)) %>%
		imap(~{mcols(.x)[.y] = 1; .x}) %>%
		map(trim)
}

import.idr = function(file, seqinfo=NULL)
{
	require(data.table)
	require(GenomicRanges)
	. = fread(file, col.names=
		  	c("seqnames", "start", "end", "id", "score", "strand",
		  	  "signalValue", "pValue", "qValue", "summit", "mLog10LocalIdr", "mLog10GlobalIdr",
		  	  "rep0_start", "rep0_end", "rep0_signalValue", "rep0_summit",
		  	  "rep1_start", "rep1_end", "rep1_signalValue", "rep1_summit" 	  ))
	.$globalIdr = 10^(-.$mLog10GlobalIdr)
	.$localIdr = 10^(-.$mLog10LocalIdr)
	ret = as(object=., Class="GRanges")
	if(!is.null(seqinfo))
		seqinfo(ret) = seqinfo
	ret
}

# to be used with purrr::reduce
mergePeaks = function(a, b, minFc=0)
{
	require(purrr)
	a = a[a$fc >= minFc]
	b = b[b$fc >= minFc]
	u = union(a, b)
	#u$fc = numeric(length(u))
	mcols(u) = mcols(a)[1, ] %>% (function(.) {.[,] = 0; .}) %>% rep(times=length(u))
	mcols(u)[setdiff(names(mcols(b)), names(mcols(a)))] = 0
	mcols(a)[setdiff(names(mcols(b)), names(mcols(a)))] = 0
	mcols(b)[setdiff(names(mcols(a)), names(mcols(b)))] = 0
	
	ova = findOverlaps(query=u, subject=a )
	agg = aggregate(mcols(a)[subjectHits(ova), ], by=list(u=queryHits(ova)), FUN=function(x) max(x)); # it cycles over DataFrame columns
	mcols(u)[agg$u, ] = agg[,-1]  # get best fc from a
	
	ovb = findOverlaps(u, b)
	agg = aggregate(mcols(b)[subjectHits(ovb), ], by=list(u=queryHits(ovb)), FUN=function(x) max(x))
	mcols(u)[agg$u, ] = names(mcols(u)) %>% `names<-`(., .) %>%
		map(~pmax(mcols(u)[agg$u, .], agg[,.])) %>% DataFrame 
	u
}
