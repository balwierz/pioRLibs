
export.bb = function(gr, file, genome="danRer10")
{
	require(rtracklayer)
	bf = paste0(file, ".bed")
	bfs = paste0(file, ".sorted.bed")
	export.bed(object=gr, con=bf)
	foo = system2("pioSortBed", bf, stdout=TRUE)
	writeLines(foo, bfs)
	system2(command="bedToBigBed", args=c(bfs,
										  paste0("/mnt/biggles/data/UCSC/goldenpath/", genome, "/chromInfo.sorted.txt"),
										  file))
	unlink(c(bf, bfs))
}
