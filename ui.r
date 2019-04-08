ui <- fluidPage(
	titlePanel("Potential assembly errors in rn6"),
	sidebarLayout(
		sidebarPanel(
			width=3,
			fluidRow(
				strong("Search by chromosomal position, M Bp"),
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
						selected=c("chr1"),
						width=130)
				),
				column(5, #offset=0, style='padding:0px;', 
					numericInput("loc", "", 80, width=80, step=1)
				)
			),
#			actionButton("positionButton","Search"),
			hr(),
			fluidRow(
				strong("Search by gene symbol, e.g. Ube3a, Ncald"),
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
							imageOutput("mvImage1"), 
							textOutput("geneList1")
						),
						column(6, offset=0, style='padding:5px;', 
							imageOutput("mvImage2"),
							textOutput("geneList2")
						)
					)
				),
				tabPanel("Legend", imageOutput("legendImage")),
				tabPanel("Method")
			)
		)
	)
)
