% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dap.R
\name{dap_crop}
\alias{dap_crop}
\title{Crop DAP file}
\usage{
dap_crop(
  URL = NULL,
  catalog = NULL,
  AOI = NULL,
  startDate = NULL,
  endDate = NULL,
  start = NULL,
  end = NULL,
  varname = NULL,
  verbose = TRUE
)
}
\arguments{
\item{URL}{local file path or URL}

\item{catalog}{subset of open.dap catalog}

\item{AOI}{an sf of SpatVect point or polygon to extract data for}

\item{startDate}{a start date given as "YYYY-MM-DD" to extract data for}

\item{endDate}{an end date given as "YYYY-MM-DD" to extract data for}

\item{start}{for non "dated" items, start can be called by index}

\item{end}{for non "dated" items, end can be called by index}

\item{varname}{variable name to extract (e.g. tmin)}

\item{verbose}{Should dap_summary be printed?}
}
\value{
data.frame
}
\description{
Crop DAP file
}
\seealso{
Other dap: 
\code{\link{.resource_grid}()},
\code{\link{.resource_time}()},
\code{\link{climater_dap}()},
\code{\link{climater_filter}()},
\code{\link{dap_get}()},
\code{\link{dap_meta}()},
\code{\link{dap_summary}()},
\code{\link{dap_to_local}()},
\code{\link{dap_xyzv}()},
\code{\link{dap}()},
\code{\link{extract_sites}()},
\code{\link{get_data}()},
\code{\link{go_get_dap_data}()},
\code{\link{grid_meta}()},
\code{\link{make_ext}()},
\code{\link{make_vect}()},
\code{\link{merge_across_time}()},
\code{\link{parse_date}()},
\code{\link{read_dap_file}()},
\code{\link{read_ftp}()},
\code{\link{time_meta}()},
\code{\link{try_att}()},
\code{\link{var_to_terra}()},
\code{\link{variable_meta}()},
\code{\link{vrt_crop_get}()}
}
\concept{dap}
