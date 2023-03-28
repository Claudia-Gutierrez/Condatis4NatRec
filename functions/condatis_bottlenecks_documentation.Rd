\name{condatis_bottlenecks}
\alias{condatis_bottlenecks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Condatis bottlenecks
}
\description{
This function calculates conductance (speed), flow, percent of total power and score for each link (potential bottlenecks) and subsets the links that account for a given percentage (threshold) of the power, or a minimum and a maximum of the highest ranked powers
}
\usage{
condatis_bottlenecks(hab, st, R, disp, filename, path)
}

\arguments{
  \item{hab}{raster of the habitat you wish to measure connectivity over}
  \item{st}{raster of location of sources and targets}
  \item{R}{value of the species moving (number of dispersers produced per km^2 of habitat)}
  \item{disp}{Dispersal distance of the species in km}
  \item{path}{path of destination folder (add '/' to end path)}
  \item{filename}{analyses name, output type will be appended at the end (e.g. filename=landscape, output= landscape_bottlenecks.shp, landscape_flow.csv,etc.)}
  \item{threshold}{percentage of the total power in the network, default = 0.99}
  \item{maxlink}{maximum number of links drawn, default = 10000}
  \item{minlink}{minimum number of links drawn, default =10}
  \item{maxdisp}{maximum distance to calculate current between cells [km], default= inf}
  \item{score_major}{interval of score of bottlenecks considered major,default = c(5,50), }
  \item{score_severe}{>score considered severe bottlenecks, default= 50}
}


\details{

\strong{Tabular outputs}

\emph{speed_power.csv}  overall speed and total power of the habitat network

'flow.csv'- flow value per pixel

'power.csv'- power values per link

---Spatial outputs

'flow_raster.tif'- raster map of flow values

'progress_raster.tif'- raster map of flow progress

'bottlenecks.shp'- shapefile map of links (lines) of potential bottlenecks, with associated power value and score (calculated as (power% × number of cells)⁄100)

'bottleneck_major_area.shp'- shapefile map of major bottleneck areas (buffers of half the length of bottleneck, around the middle point of major, merges the overlapping features and calculates the sum of their scores). Note: produced only if there are links within the 'score_major' interval (see 'Default values')

'bottleneck_sever_area.shp'- shapefile map of severe bottleneck areas (buffers of half the length of bottleneck, around the middle point of severe, merges the overlapping features and calculates the sum of their scores). Note: produced only if there are links with score above 'score_severe' (see 'Default values')
}
\value{
%%  %%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Hodgson, J., Allen, K., & Cole, L. (2022). Help Document for Condatis Version 1.2. \href{https://pgb.liv.ac.uk/~condatis_dev/help/help.htm}{https://pgb.liv.ac.uk/~condatis_dev/help/help.htm}

Hodgson, J. A., Thomas, C. D., Dytham, C., Travis, J. M. J., & Cornell, S. J. (2012). The Speed of Range Shifts in Fragmented Landscapes. PLoS ONE, 7(10), e47141. \href{10.1371/journal.pone.0047141}{https://doi.org/10.1371/journal.pone.0047141} 

Hodgson, J. A., Wallis, D. W., Krishna, R., Cornell, S. J., & Isaac, N. (2016). How to manipulate landscapes to improve the potential for range expansion. Methods in Ecology and Evolution, 7(12), 1558-1566. 10.1111/2041-210x.12614 \href{https://doi.org/10.1111/2041-210X.12614}{https://doi.org/10.1111/2041-210X.12614}

}
\author{
Jenny Hodgson, Claudia Gutierrez-Arellano, Thomas Travers
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
