# synergetr

An R package to generate synthetic data with empirical probability distributions.

## Why Synthetic Data?

Real data might be unavailable for analysis due to

* limiting legal or business rules, or because the
* data does not exist yet. 

In the first case, the problem can be alleviated by *pseudonymization* (changing
identifiers into codes that are kept safe) or by *anonymization*, where the data is
scrabled beyond recognition (even for those who performed this operation).  In the
absence of real data, one can *fabricate data*, for example  by running a dynamical
simulation model.

The **synergetr** package **draws data from statistical distributions**, which is yet
another option. The package contains several functions to compute *empirical probability
distributions* from the original (categorical or continuous) data, and to fabricate new
random data out of it.  Since the generated data is synthetic, it can usually be used
without restrictions. For example, synthetic timestamps can follow the original
statistical distribution (e.g. rush in the morning), but finding an
exact time point like '2017-08-05 09:08:19' from the original data is highly unlikely. 

The computations rely on the algorithms build in the [R language](http://r-project.org),
but the functions add convenience for processing different data types and sensible
defaults for various options. In addition, computing the *empirical probability
distribution function* (EPDF) for multidimensional data can be computationally expensive.
Currently, *synergetr* supports in-memory computations through
[*data.table*](https://cran.r-project.org/web/packages/data.table/index.html) package,
and using [PostgreSQL](https://www.postgresql.org/) database for better performance
database.  (Hadoop and SQL Server support should be easy to implement, but they are not
there yet...)


## Installation

Note: *data.table* is a dependency for *synergetr* (and needs to be installed, even
though it is used only for the in-memory option).

    install.packaged("data.table")

#### Install Directly from GitHub using *devtools*

    install.packages("devtools")
    devtools::install_github("avirkki/synergetr")

#### Download the Bundled Package

Download the latest release [from here](http://cci.vsshp.fi/synergetr/) and install it
as a local package through GUI menus or at command line with 

    R CMD INSTALL synergetr_<version>.tar.gz


## Usage

| Function       | Type            | Examples             |
|:-------------- |:----------------|:---------------------|
| rfactor        | factors         | male, female         |
| rnumeric       | numeric data    | 1,2,3,..,; 3.76123   |
| rcode          | code values     | hQA3RC, 00121        |
| rtime          | times ISO-8601  | 2017-06-01 14:02:47  |
| rdate          | dates ISO-8601  | 2017-08-05           |
| ...            | ...             | ...                  |

To see the full list of functions that generate random data togeher with some other data
exploration tools, issue synergetr:: \<tab key\> in the R console.

## Examples

    library("synergetr")
    data(diags)
    help(diags)

    > rfactor(diags, c("home_town","home_town_code"), 5)
    Generating home_town home_town_code ...
        home_town home_town_code
    1       TURKU            853
    11    TAMPERE            837
    14     LOIMAA            430
    116  MAALAHTI            475
    147    KARVIA            230

    > rtime(diags, "diag_date", 5)
    Generating diag_date ...
                diag_date
    1 2006-01-08 11:21:56
    2 2004-12-19 07:53:38
    3 2013-03-26 22:11:00
    4 2016-09-19 01:17:26


## Further Examples

See the [datafabr](https://github.com/avirkki/datafabr/tree/master/scripts) repository
for real-life scripts to process clinical data. 
