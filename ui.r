ui <- fluidPage(
	titlePanel("Potential assembly errors in rn6"),
	sidebarLayout(
		sidebarPanel(
			width=3,
			fluidRow(
				column(3,
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
						"chr20"="chr20" ),
						selected=c("chr1"),
						width=80)
				),
				column(4, numericInput("loc", "From, M bp", 80, width=100, step=2 ))
			)
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Matrix View", 
					fluidRow( 
							column(6, offset=0, style='padding:5px;', imageOutput("mvImage1" )), 
							column(6, offset=0, style='padding:5px;', imageOutput("mvImage2"))
							),
					fluidRow(
							column(6, offset=0, style='padding:5px;', imageOutput("mvImage3")),
							column(6, offset=0, style='padding:5px;', imageOutput("mvImage4"))
							)
					),
				tabPanel("Legend", imageOutput("legendImage")),
				tabPanel("Method")
			)
		)
	)
)
