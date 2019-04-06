library("shiny")

server <- function(input, output, session) {
	pad=c("","0","00")
	output$mvImage1 <- renderImage({
		chr=input$chr
		mbpStart=input$loc
		pad0=4-nchar(mbpStart)
		mbpStart=paste(pad[pad0], mbpStart, sep="")
		imgName=paste("./pngs/sv_",chr,"_", mbpStart,"000001-", mbpStart,"500000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	}, deleteFile = FALSE)

	output$mvImage2 <- renderImage({
		chr=input$chr
		mbpStart=input$loc
		mbpEnd=input$loc+1
		pad0=4-nchar(mbpStart)
		mbpStart=paste(pad[pad0], mbpStart, sep="")
		mbpEnd=paste(pad[pad0], mbpEnd, sep="")
		imgName=paste("./pngs/sv_",chr,"_", mbpStart,"500001-", mbpEnd,"000000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	}, deleteFile = FALSE)

	output$mvImage3 <- renderImage({
		chr=input$chr
		mbpStart=input$loc+1
		pad0=4-nchar(mbpStart)
		mbpStart=paste(pad[pad0], mbpStart, sep="")
		imgName=paste("./pngs/sv_",chr,"_", mbpStart,"000001-", mbpStart,"500000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	}, deleteFile = FALSE)

	output$mvImage4 <- renderImage({
		chr=input$chr
		mbpStart=input$loc+1
		mbpEnd=input$loc+2
		pad0=4-nchar(mbpStart)
		mbpStart=paste(pad[pad0], mbpStart, sep="")
		mbpEnd=paste(pad[pad0], mbpEnd, sep="")
		imgName=paste("./pngs/sv_",chr,"_", mbpStart,"500001-", mbpEnd,"000000_bn_both.png", sep="")
		list(src = imgName,
			contentType = 'image/png',
			width=450,
			alt = imgName)
	},deleteFile = FALSE)

	output$legendImage <- renderImage({
		imgName="rotated_matrix_view.png"
		list(src = imgName,
			contentType = 'image/png',
			width=550,
			alt = imgName)
	},deleteFile = FALSE)

}

