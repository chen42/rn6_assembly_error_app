library("shiny")

genes<-read.table(file="./rn6_gene_pos_symb.tab",sep="\t", header=T)
pad=c("00","000","0000")

server <- function(input, output, session) {
	target<-eventReactive(input$submitButton, {
		inputsymb=gsub(" ", "",input$geneSymb)
		if (nchar(inputsymb)>1){
			idx<-toupper(inputsymb) == toupper(genes$symb)
			if (sum(idx)==1){
				gene0=genes[idx,]
				mbpStart=floor(gene0$start/1e6)
				chr=gene0$chr
				x1=gene0$start-mbpStart*1e+6
				x2=gene0$end-mbpStart*1e+6
				x1=(x1*1052/5e+5)-10
				x2=(x2*1052/5e+5)-10
			} else if (sum(idx)>1){
				# multiple hits
				chr="chr1"
				mbpStart=1
				## steal x1/x2 as the error code
				x1=-20
				x2=-20
			} else {
				# no hits
				chr="chr1"
				mbpStart=1
				x1=-10
				x2=-10
			}
		} else {
			chr=input$chr
			mbpStart=input$loc
			if (floor(mbpStart) != mbpStart) { 
				bpStart=mbpStart*1e+6
				x1=bpStart-floor(mbpStart)*1e+6
				x1=(x1*1052/5e+5)-10
				x2=x1
				mbpStart=floor(mbpStart)
				inputsymb=paste(input$loc, "M bp")
			} else {
				x1= -1 
				x2= -1 
			}
		}

		#x1 and x2 are position for the gene (pixel) on the x-axis 
		data.frame(chr=chr, start=mbpStart, x1=x1, x2=x2, symb=inputsymb)
	})
	
	get_symbs<-function(chr0,start0,end0, legend0){
		inRange=subset(genes, as.character(chr)==as.character(chr0) & ((start >start0 & start <end0 ) | (end > start0 & end < end0))) 
		mbp1<-format(start0/1e+6,nsmall=1)
		mbp2<-format(end0/1e+6,nsmall=1)
		symbs<- as.character(unique(inRange[order(inRange$start),"symb"]))
		textout<-c("Genes on ", as.character(chr0), "between", mbp1, "–", mbp2, "Mbp:", symbs) #, "x1=", target()$x1, "x2=", target()$x2)
		if (legend0==TRUE){
			textout<-c("The location of", as.character(target()$symb), "is indicated using blue brackets. ", textout)
		}
		if (target()$x1== -10){
			textout<-c("Your search" , as.character(target()$symb), "is not found. ", textout)
		} else if(target()$x1== -20){
			textout<-c("Your search" , as.character(target()$symb), "has multiple hits. Please specify chromosomal location instead. ", textout)
		}
		textout
	}

	generate_img<-function(chr, start, end, side){
		imgName=paste("./pngs/",chr,"_", start,"00001-", end,"00000_bn_multi_combined.png", sep="")
		if (side=='left') {
			try(system(paste("cp", imgName, "leftImg.png")))
		} else {
			try(system(paste("cp", imgName, "rightImg.png")))
		}
		x1=target()$x1
		x2=target()$x2
		## annotate the image with the gene of interest, 1052 is the width of the image
		if (x1 > 0  & x1 < 1052) {
			system(paste("convert leftImg.png +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x1, "-300 \"[\" leftImg.png", sep=""))
		}
		if (x2 > 0  & x2 < 1052) {
			system(paste("convert leftImg.png +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x2, "-300 \"]\" leftImg.png", sep=""))
		}

		if (x1 > 1052) {
			x1 <- x1-1052
			system(paste("convert rightImg.png +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x1, "-300 \"[\" rightImg.png", sep=""))
		}
		if (x2 > 1052) {
			x2 <- x2-1052
			system(paste("convert rightImg.png +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x2, "-300 \"]\" rightImg.png", sep=""))
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
			width=input$imgSize,
			alt = "leftImg.png")
	}, deleteFile = TRUE)

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
		generate_img(t0$chr, mbpMid, mbpEnd, side="right")
		list(src = "rightImg.png",
			contentType = 'image/png',
			width=input$imgSize,
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
