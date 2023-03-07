// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_envelope_z
Rcpp::NumericVector get_envelope_z(Rcpp::List sf, int depth);
RcppExport SEXP _arcgis_get_envelope_z(SEXP sfSEXP, SEXP depthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type sf(sfSEXP);
    Rcpp::traits::input_parameter< int >::type depth(depthSEXP);
    rcpp_result_gen = Rcpp::wrap(get_envelope_z(sf, depth));
    return rcpp_result_gen;
END_RCPP
}
// get_envelope_zm
Rcpp::NumericVector get_envelope_zm(Rcpp::List sf, int depth);
RcppExport SEXP _arcgis_get_envelope_zm(SEXP sfSEXP, SEXP depthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type sf(sfSEXP);
    Rcpp::traits::input_parameter< int >::type depth(depthSEXP);
    rcpp_result_gen = Rcpp::wrap(get_envelope_zm(sf, depth));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_xy
List sfc_point_xy(const List points);
RcppExport SEXP _arcgis_sfc_point_xy(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xy(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_xyz
List sfc_point_xyz(const List points);
RcppExport SEXP _arcgis_sfc_point_xyz(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xyz(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_xyzm
List sfc_point_xyzm(const List points);
RcppExport SEXP _arcgis_sfc_point_xyzm(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_xyzm(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_point_impl
List sfc_point_impl(const List points);
RcppExport SEXP _arcgis_sfc_point_impl(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_point_impl(points));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multipoint_impl
List sfc_multipoint_impl(const List mpoints);
RcppExport SEXP _arcgis_sfc_multipoint_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multipoint_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_linestring_impl
List sfc_linestring_impl(const List mpoints);
RcppExport SEXP _arcgis_sfc_linestring_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_linestring_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multilinestring_inner_impl
List sfc_multilinestring_inner_impl(List mpoints);
RcppExport SEXP _arcgis_sfc_multilinestring_inner_impl(SEXP mpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mpoints(mpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multilinestring_inner_impl(mpoints));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multilinestring_impl
List sfc_multilinestring_impl(const List mlines);
RcppExport SEXP _arcgis_sfc_multilinestring_impl(SEXP mlinesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mlines(mlinesSEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multilinestring_impl(mlines));
    return rcpp_result_gen;
END_RCPP
}
// sfg_polygon_impl
List sfg_polygon_impl(List mply);
RcppExport SEXP _arcgis_sfg_polygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_polygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfc_polygon_impl
List sfc_polygon_impl(const List mply);
RcppExport SEXP _arcgis_sfc_polygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_polygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfg_multipolygon_inner_impl
List sfg_multipolygon_inner_impl(const List mply);
RcppExport SEXP _arcgis_sfg_multipolygon_inner_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_multipolygon_inner_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfg_multipolygon_impl
List sfg_multipolygon_impl(List mply);
RcppExport SEXP _arcgis_sfg_multipolygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfg_multipolygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// sfc_multipolygon_impl
List sfc_multipolygon_impl(List mply);
RcppExport SEXP _arcgis_sfc_multipolygon_impl(SEXP mplySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mply(mplySEXP);
    rcpp_result_gen = Rcpp::wrap(sfc_multipolygon_impl(mply));
    return rcpp_result_gen;
END_RCPP
}
// transpose_cpp
SEXP transpose_cpp(SEXP x, SEXP names_template);
RcppExport SEXP _arcgis_transpose_cpp(SEXP xSEXP, SEXP names_templateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type names_template(names_templateSEXP);
    rcpp_result_gen = Rcpp::wrap(transpose_cpp(x, names_template));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_arcgis_get_envelope_z", (DL_FUNC) &_arcgis_get_envelope_z, 2},
    {"_arcgis_get_envelope_zm", (DL_FUNC) &_arcgis_get_envelope_zm, 2},
    {"_arcgis_sfc_point_xy", (DL_FUNC) &_arcgis_sfc_point_xy, 1},
    {"_arcgis_sfc_point_xyz", (DL_FUNC) &_arcgis_sfc_point_xyz, 1},
    {"_arcgis_sfc_point_xyzm", (DL_FUNC) &_arcgis_sfc_point_xyzm, 1},
    {"_arcgis_sfc_point_impl", (DL_FUNC) &_arcgis_sfc_point_impl, 1},
    {"_arcgis_sfc_multipoint_impl", (DL_FUNC) &_arcgis_sfc_multipoint_impl, 1},
    {"_arcgis_sfc_linestring_impl", (DL_FUNC) &_arcgis_sfc_linestring_impl, 1},
    {"_arcgis_sfc_multilinestring_inner_impl", (DL_FUNC) &_arcgis_sfc_multilinestring_inner_impl, 1},
    {"_arcgis_sfc_multilinestring_impl", (DL_FUNC) &_arcgis_sfc_multilinestring_impl, 1},
    {"_arcgis_sfg_polygon_impl", (DL_FUNC) &_arcgis_sfg_polygon_impl, 1},
    {"_arcgis_sfc_polygon_impl", (DL_FUNC) &_arcgis_sfc_polygon_impl, 1},
    {"_arcgis_sfg_multipolygon_inner_impl", (DL_FUNC) &_arcgis_sfg_multipolygon_inner_impl, 1},
    {"_arcgis_sfg_multipolygon_impl", (DL_FUNC) &_arcgis_sfg_multipolygon_impl, 1},
    {"_arcgis_sfc_multipolygon_impl", (DL_FUNC) &_arcgis_sfc_multipolygon_impl, 1},
    {"_arcgis_transpose_cpp", (DL_FUNC) &_arcgis_transpose_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_arcgis(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
