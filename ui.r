ui <- fluidPage(
	titlePanel("Potential assembly errors in rn6"),
	tags$head(tags$style( type="text/css", 
						"#mvImage1 img {max-width:100%; width:100%; height:auto}
						 #mvImage2 img {max-width:100%; width:100%; height:auto} 
						 #svsImage1 img {max-width:100%; width:100%; height:auto} 
						 #svsImage2 img {max-width:100%; width:100%; height:auto} 
						")), 
	#tags$head( tags$link(rel="stylesheet", type="text/css", href="rn6_err.css")),
	sidebarLayout(
		sidebarPanel(
			width=3,
			fluidRow(
				strong("Search by Chr location, M bp"),
				column(5, #offset=0, style='padding:0px;',
					selectInput("chr", "", 
						c("chr1"="chr1",
						"chr2"="chr2", 
						"chr3"="chr3", 
						"chr4"="chr4",
						"chr5"="chr5",
						"chr6"="chr6",
						"chr7"="chr7",
						"chr8"="chr8",
						"chr9"="chr9",
						"chr10"="chr10",
						"chr11"="chr11",
						"chr12"="chr12",
						"chr13"="chr13",
						"chr14"="chr14",
						"chr15"="chr15",
						"chr16"="chr16",
						"chr17"="chr17",
						"chr18"="chr18",
						"chr19"="chr19",
						"chr20"="chr20",
						"chrX"="chrX",
						"chrY"="chrY"),
						selected=c("chr12"),
						width=130)
				),
				column(5, #offset=0, style='padding:0px;', 
					numericInput("loc", "", 8.4, width=80, min=0, max=283,step=1)
				)
			),
			fluidRow(
				strong("Search by gene symbol, e.g. Ube3a, Ncald, Exoc6b"),
				column(12, 
					textInput("geneSymb", "", "", width=250)
				)
			),
			actionButton("submitButton","Search")
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Matrix View", 
					fluidRow(
						column(6, offset=0, style='padding:5px;', 
							textOutput("geneList1")
						),
						column(6, offset=0, style='padding:5px;', 
							textOutput("geneList2")
						)
					),
					fluidRow( 
						column(6, offset=0, style='padding:5px;', 
							imageOutput("mvImage1")
						),
						column(6, offset=0, style='padding:5px;', 
							imageOutput("mvImage2") 
						)
					)
				),
				tabPanel("Legend", imageOutput("legendImage")),
				tabPanel("Method")
			)
		)
	)
)
