library("shiny")
library("ggplot2")

genes<-read.table(file="./rn6_gene_pos_symb.tab",sep="\t", header=T)
pad=c("00","000","0000")

# data for SVs from inbreds
svs<-read.table(file="svs.tab", sep="\t")
names(svs)<-c("SV_Type","sv_chr","sv_start","call", "sv_end","qual")
svs$sv_start<-svs$sv_start/1e6
svs$sv_end<-svs$sv_end/1e6

server <- function(input, output, session) {
	# get the target region, include chr, star location, gene symb.
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
		#x1 and x2 are position for the gene (pixel) on the x-axis, it is also used as error code 
		data.frame(chr=chr, start=mbpStart, x1=x1, x2=x2, symb=inputsymb)
	})
	# get the genes found in the region	
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
	# generate the images
	leftImg=tempfile(fileext=".png")
	rightImg=tempfile(fileext=".png")
	generate_img<-function(side){
		t0=target()
		chr=t0$chr
		x1=t0$x1
		x2=t0$x2
		if (side=='left') {
			mbpstart=t0$start
			mbpend=t0$start+0.5
			loc1=paste(pad[4-nchar(t0$start)], t0$start,0, sep="")
			loc2=paste(pad[4-nchar(t0$start)], t0$start,5, sep="")
			imgName=paste("./pngs/",chr,"_", loc1,"00001-", loc2,"00000_bn_multi_combined.png", sep="")
			try(system(paste("cp", imgName, leftImg)))
		} else {
			mbpstart=t0$start+0.5
			mbpend=t0$start+1
			loc1=paste(pad[4-nchar(t0$start)], t0$start,5, sep="")
			loc2=paste(pad[4-nchar(t0$start+1)], t0$start+1,0, sep="")
			imgName=paste("./pngs/",chr,"_", loc1,"00001-", loc2,"00000_bn_multi_combined.png", sep="")
			try(system(paste("cp", imgName, rightImg)))
		}
		## SV track for first image
		y_sv=1
		svtrack=tempfile(fileext='.png')
		svsdf<- subset(svs, as.character(sv_chr)==as.character(chr) & ((sv_end > mbpstart & sv_end < mbpend) | (sv_start>mbpstart & sv_start<mbpend))) 
		if (dim(svsdf)[1]==0){
			# avoid the error message 
			svsdf<-data.frame(SV_Type="del",sv_chr="chr",sv_start=0.1, sv_end=0.1, pass="PASS")
		}
		png(file=svtrack,width=1052,height=250)
		p<-ggplot(data=svsdf, aes(x=sv_start, xend=sv_end, y=y_sv, yend=y_sv, color=SV_Type))+
		geom_segment(size=30) + 
		scale_color_manual(values=c("DUP"="#4283f430", "INV"="#f2360330","DEL"="#1c1c1c30"))+ 
		ylim(.999,1.001)+
		ylab("")+
		xlim(mbpstart, mbpend)+
		xlab("Mb")+
		theme_bw()+ 
		theme(legend.position="bottom", text=element_text(size=30), panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
		theme(plot.margin=margin(0,-52,0,-150)) #(top, right, bottom, left)
		print(p)
		dev.off()
		## annotate the image with the gene of interest, 1052 is the width of the image
		if (x1 > 0 & x1 < 1052) {
			system(paste("convert ",  leftImg, " +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x1, "-300 \"[\" ",  leftImg, sep=""))
		}
		if (x2 > 0 & x2 < 1052) {
			system(paste("convert ", leftImg, " +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x2, "-300 \"]\" ",  leftImg, sep=""))
		}
		if (x1 > 1052) {
			x1 <- x1-1052
			system(paste("convert ", rightImg, " +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x1, "-300 \"[\" ",  rightImg, sep=""))
		}
		if (x2 > 1052) {
			x2 <- x2-1052
			system(paste("convert ", rightImg, " +repage -gravity west -pointsize 80 -fill royalblue2 -annotate +", x2, "-300 \"]\" ", rightImg, sep=""))
		}
		# append the svtrack
		if (side=='left') {
			system(paste("convert", leftImg, svtrack, " -append", leftImg, sep=" "))
		} else {
			system(paste("convert", rightImg, svtrack, " -append", rightImg, sep=" "))
		}
	}
	
	#first image
	output$mvImage1<-renderImage({
		generate_img(side="left")
		list(src = leftImg,
			contentType = 'image/png',
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
		generate_img(side="right")
		list(src = rightImg,
			contentType = 'image/png',
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
