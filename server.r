library("shiny")

genes<-read.table(file="./rn6_gene_pos_symb.tab",sep="\t", header=T)
pad=c("","0","00")

server <- function(input, output, session) {

	target<-eventReactive(input$submitButton, {
		if (nchar(input$geneSymb)>1){
			idx<-toupper(input$geneSymb) == toupper(genes$symb)
			if (sum(idx)==1){
				gene0=genes[idx,]
				mbpStart=floor(gene0$start/1e6)
			}
			chr=gene0$chr
		} else {
			chr=input$chr
			mbpStart=input$loc
		}
		data.frame(chr=chr, start=mbpStart)
	})
	
	get_symbs<-function(chr0,start0,end0){
		inRange=subset(genes, as.character(chr)==as.character(chr0) & ((start >start0 & start <end0 ) | (end > start0 & end < end0))) 
		#inRange=subset(genes,(start >start0 & start <end0 ) | (end > start0 & end < end0)) 
		mbp1<-format(start0/1e+6,nsmall=1)
		mbp2<-format(end0/1e+6,nsmall=1)
		c("Genes on ", as.character(chr0), "between", mbp1, "-", mbp2, "Mbp:", as.character(inRange[order(inRange$start),"symb"]))
	}
	
	#first image
	output$mvImage1<-renderImage({
		t0=target()
		pad0=4-nchar(t0$start)
		mbpBegin=paste(pad[pad0], t0$start, 0, sep="")
		mbpMid=paste(pad[pad0], t0$start,5, sep="")
		imgName=paste("./pngs/sv_",t0$chr,"_", mbpBegin,"00001-", mbpMid,"00000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	}, deleteFile = FALSE)
	# legend for first image		
	output$geneList1<-renderText({
		t0<-target()
		bpStart<-t0$start*1e6	
		bpEnd<-bpStart+5e+5
		get_symbs(t0$chr,bpStart,bpEnd)
	})
	# second image
	output$mvImage2<-renderImage({
		t0=target()
		pad0=4-nchar(t0$start)
		mbpMid=paste(pad[pad0], t0$start,5, sep="")
		mbpEnd=paste(pad[pad0], t0$start+1, 0, sep="")
		imgName=paste("./pngs/sv_",t0$chr,"_", mbpMid,"00001-", mbpEnd, "00000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	}, deleteFile = FALSE)
	# legend for second image
	output$geneList2<-renderText({
		t0<-target()
		bpStart<-(t0$start+0.5)*1e6	
		bpEnd<-bpStart+5e+5
		get_symbs(t0$chr,bpStart,bpEnd)
	})
	# set the default chr, Mbp to the location of the searched gene, clear input field 
	observe({
		updateSelectInput(session, "chr", selected=target()$chr)
		updateNumericInput(session, "loc", label="M bp", value=target()$start)
		updateTextInput(session, "geneSymb", value="")
	})
	## legend panel
	output$legendImage <- renderImage({
		imgName="rotated_matrix_view.png"
		list(src = imgName,
			contentType = 'image/png',
			width=550,
			alt = imgName)
	},deleteFile = FALSE)

}
