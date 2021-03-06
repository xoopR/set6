% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Set_Interval_SpecialSet.R
\name{Rationals}
\alias{Rationals}
\title{Set of Rational Numbers}
\description{
The mathematical set of rational numbers,
defined as the set of numbers that can be written as a fraction of two integers. i.e.
\deqn{\\{\frac{p}{q} \ : \ p,q \ \in \ Z, \ q \ne 0 \\}}{p/q : p,q \epsilon Z, q != 0}
where \eqn{Z} is the set of integers.
}
\details{
The \verb{$contains} method does not work for the set of Rationals as it is notoriously
difficult/impossible to find an algorithm for determining if any given number is rational or not.
Furthermore, computers must truncate all irrational numbers to rational numbers.
}
\seealso{
Other special sets: 
\code{\link{Complex}},
\code{\link{ExtendedReals}},
\code{\link{Integers}},
\code{\link{Logicals}},
\code{\link{Naturals}},
\code{\link{NegIntegers}},
\code{\link{NegRationals}},
\code{\link{NegReals}},
\code{\link{PosIntegers}},
\code{\link{PosNaturals}},
\code{\link{PosRationals}},
\code{\link{PosReals}},
\code{\link{Reals}},
\code{\link{Universal}}
}
\concept{special sets}
\section{Super classes}{
\code{\link[set6:Set]{set6::Set}} -> \code{\link[set6:Interval]{set6::Interval}} -> \code{\link[set6:SpecialSet]{set6::SpecialSet}} -> \code{Rationals}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Rationals$new()}}
\item \href{#method-contains}{\code{Rationals$contains()}}
\item \href{#method-isSubset}{\code{Rationals$isSubset()}}
\item \href{#method-equals}{\code{Rationals$equals()}}
\item \href{#method-clone}{\code{Rationals$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Set" data-id="add">}\href{../../set6/html/Set.html#method-add}{\code{set6::Set$add()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Set" data-id="multiplicity">}\href{../../set6/html/Set.html#method-multiplicity}{\code{set6::Set$multiplicity()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Set" data-id="print">}\href{../../set6/html/Set.html#method-print}{\code{set6::Set$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Set" data-id="remove">}\href{../../set6/html/Set.html#method-remove}{\code{set6::Set$remove()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Set" data-id="summary">}\href{../../set6/html/Set.html#method-summary}{\code{set6::Set$summary()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="Interval" data-id="isSubinterval">}\href{../../set6/html/Interval.html#method-isSubinterval}{\code{set6::Interval$isSubinterval()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="set6" data-topic="SpecialSet" data-id="strprint">}\href{../../set6/html/SpecialSet.html#method-strprint}{\code{set6::SpecialSet$strprint()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{Rationals} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Rationals$new(lower = -Inf, upper = Inf, type = "()")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lower}}{numeric. Where to start the set. Advised to ignore, used by child-classes.}

\item{\code{upper}}{numeric. Where to end the set. Advised to ignore, used by child-classes.}

\item{\code{type}}{character Set closure type. Advised to ignore, used by child-classes.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Rationals} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-contains"></a>}}
\if{latex}{\out{\hypertarget{method-contains}{}}}
\subsection{Method \code{contains()}}{
Method not possible for Rationals.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Rationals$contains(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-isSubset"></a>}}
\if{latex}{\out{\hypertarget{method-isSubset}{}}}
\subsection{Method \code{isSubset()}}{
Method not possible for Rationals.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Rationals$isSubset(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-equals"></a>}}
\if{latex}{\out{\hypertarget{method-equals}{}}}
\subsection{Method \code{equals()}}{
Method not possible for Rationals.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Rationals$equals(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Rationals$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
