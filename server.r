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
			x1=gene0$start-mbpStart*1e+6
			x2=gene0$end-mbpStart*1e+6
			x1=(x1*1048/5e+5)+10
			x2=(x2*1048/5e+5)+10
		} else {
			chr=input$chr
			mbpStart=input$loc
			x1= -1 
			x2= -1 
		}

		#x1 and x2 are position for the gene (pixel) on the x-axis 
		data.frame(chr=chr, start=mbpStart, x1=x1, x2=x2, symb=input$geneSymb)
	})
	
	get_symbs<-function(chr0,start0,end0, legend0){
		inRange=subset(genes, as.character(chr)==as.character(chr0) & ((start >start0 & start <end0 ) | (end > start0 & end < end0))) 
		mbp1<-format(start0/1e+6,nsmall=1)
		mbp2<-format(end0/1e+6,nsmall=1)
		symbs<- as.character(unique(inRange[order(inRange$start),"symb"]))
		textout<-c("Genes on ", as.character(chr0), "between", mbp1, "-", mbp2, "Mbp:", symbs)
		if (legend0==TRUE){
			textout<-c("The location of", as.character(target()$symb), "is indicated using blue brackets. ", textout)
		}
		textout
	}

	generate_img<-function(chr, start, end, side){
		imgName=paste("./pngs/sv_",chr,"_", start,"00001-", end,"00000_bn_both.png", sep="")
		if (side=='left') {
			try(system(paste("cp", imgName, "leftImg.png")))
		} else {
			try(system(paste("cp", imgName, "rightImg.png")))
		}
		x1=target()$x1
		x2=target()$x2
		## annotate the image with the gene of interest
		if (x1 >0  & x1 < 1048) {
			system(paste("convert leftImg.png -gravity west -pointsize 100 -fill royalblue2 -annotate +", x1, "+0 \"[\" leftImg.png", sep=""))
		}
		if (x2 >0  & x2 < 1048) {
			system(paste("convert leftImg.png -gravity west -pointsize 100 -fill royalblue2 -annotate +", x2, "+0 \"]\" leftImg.png", sep=""))
		}
		if (x1>1048) {
			x1<-x1-1048
			system(paste("convert rightImg.png -gravity west -pointsize 100 -fill royalblue2 -annotate +", x1, "+0 \"[\" rightImg.png", sep=""))
		}
		if (x2>1048) {
			x2<-x2-1048
			system(paste("convert rightImg.png -gravity west -pointsize 100 -fill royalblue2 -annotate +", x2, "+0 \"]\" rightImg.png", sep=""))
		}
	
	}
	
	#first image
	output$mvImage1<-renderImage({
		t0=target()
		pad0=4-nchar(t0$start)
		mbpBegin=paste(pad[pad0], t0$start, 0, sep="")
		mbpMid=paste(pad[pad0], t0$start,5, sep="")
		generate_img(t0$chr, mbpBegin, mbpMid, side="left")
		list(src = "leftImg.png",
			contentType = 'image/png',
			width=450,
			alt = "leftImg.png")
	}, deleteFile = FALSE)
	# legend for first image		
	output$geneList1<-renderText({
		t0<-target()
		bpStart<-t0$start*1e6	
		bpEnd<-bpStart+5e+5
		showLegend<-t0$x1>0 & t0$x1<1048
		get_symbs(t0$chr,bpStart,bpEnd, showLegend)
	})

	# second image
	output$mvImage2<-renderImage({
		t0=target()
		pad0=4-nchar(t0$start)
		mbpMid=paste(pad[pad0], t0$start,5, sep="")
		mbpEnd=paste(pad[pad0], t0$start+1, 0, sep="")
		imgName=paste("./pngs/sv_",t0$chr,"_", mbpMid,"00001-", mbpEnd, "00000_bn_both.png", sep="")
		generate_img(t0$chr, mbpMid, mbpEnd, side="right")
		list(src = "rightImg.png",
			contentType = 'image/png',
			width=450,
			alt = "rightImg.png")
	}, deleteFile = TRUE)
	# legend for second image
	output$geneList2<-renderText({
		t0<-target()
		bpStart<-(t0$start+0.5)*1e6	
		bpEnd<-bpStart+5e+5
		showLegend<-t0$x1>1024 | t0$x2>1048
		get_symbs(t0$chr,bpStart,bpEnd, showLegend)
	})
	# set the default chr, Mbp to the location of the searched gene, clear input field 
	observe({
		updateSelectInput(session, "chr", selected=target()$chr)
		updateNumericInput(session, "loc", value=target()$start)
		updateTextInput(session, "geneSymb", value="")	})
	## legend panel
	output$legendImage <- renderImage({
		imgName="rotated_matrix_view.png"
		list(src = imgName,
			contentType = 'image/png',
			width=550,
			alt = imgName)
	},deleteFile = FALSE)

}
