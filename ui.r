ui <- fluidPage(
	titlePanel("Potential assembly errors in rn6"),
	sidebarLayout(
		sidebarPanel(
			width=3,
			fluidRow(
				column(3, offset=0, style='padding:5px;',
					selectInput("chr", "Chr", 
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
				column(3, offset=0, style='padding:5px;', 
					 numericInput("loc", "M bp", 80, width=120, step=1)
				)
			),
			fluidRow(textInput("geneSymb", "Search by gene symbol, e.g. Gfm2, Ncald", "", width=250)),
			actionButton("submitButton","submit")
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
