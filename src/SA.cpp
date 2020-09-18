// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// arma::sp_mat sp_rows(arma::sp_mat A,
//                      arma::uvec r   ){
//   uword n_rows   = A.n_rows;
//   //  uword n_cols   = A.n_cols;
//   uword n_r      = r.size();
//   uvec  tmp      = regspace<uvec>(0,n_rows-1);
//   tmp      = tmp.elem(r);
//   umat  location = join_vert(trans(regspace<uvec>(0,n_r-1)),trans(tmp));
//   sp_mat J(location,ones<vec>(n_r),n_r,n_rows);
//   sp_mat C       = J*A;
//   return(C);
// }
//
// arma::sp_mat sp_cols(arma::sp_mat A,
//                      arma::uvec r   ){
//   //  uword n_rows   = A.n_rows;
//   uword n_cols   = A.n_cols;
//   uword n_r      = r.size();
//   uvec  tmp      = regspace<uvec>(0,n_cols-1);
//   tmp            = tmp.elem(r);
//   umat  location = join_vert(trans(tmp),trans(regspace<uvec>(0,n_r-1)));
//   sp_mat J(location,ones<vec>(n_r),n_cols,n_r);
//   sp_mat C       = A*J;
//   return(C);
// }

// // [[Rcpp::export]]
// arma::uvec get_ar_lags(arma::uword lag_length,
//                        arma::uvec s_lag){
//   uvec one(lag_length+1, fill::ones);
//   uvec out = s_lag(0)*one - regspace<uvec>(0,lag_length);
//   uvec ind;
//   for(uword k=1; k<s_lag.size(); k++){
//     out = join_vert(out, s_lag(k)*one - regspace<uvec>(0,lag_length));
//   }
//   return(out);
// }

//Stack times series data in VAR format
// [[Rcpp::export]]
arma:: mat stack_obs(arma::mat nn, arma::uword p, arma::uword r = 0){
  uword rr = nn.n_rows;
  uword mn = nn.n_cols;
  if(r == 0){
    r = rr-p+1;
  }
  if(rr-p+1 != r){
    stop("Length of input nn and length of data r do not agree.");
  }
  mat N(r,mn*p, fill::zeros);
  uword indx = 0;
  for(uword j = 1; j<=p; j++){
    N.cols(indx,indx+mn-1) = nn.rows(p-j,rr-j);
    indx = indx+mn;
  }
  return(N);
}

// //Create the companion form of the transition matrix B
// // [[Rcpp::export]]
// arma::mat comp_form(arma::mat B){
//   uword r = B.n_rows;
//   uword c = B.n_cols;
//   mat A   = join_vert(B, join_horiz(eye<mat>(c-r,c-r), zeros<mat>(c-r,r)));
//   return(A);
// }

// [[Rcpp::export]]
Rcpp::Date last_year_holiday(Rcpp::Date today,
                             Rcpp::Date this_holiday,
                             Rcpp::Date last_holiday){
  int ddiff = today-this_holiday;
  Date h_date = last_holiday + ddiff;
  return h_date; // 2000-01-02
}

// [[Rcpp::export]]
Rcpp::Date last_year(Rcpp::Date today){
  Date d(today.getYear()-1,today.getMonth() ,today.getDay());
  //int ddiff = today-d;
  return d; // 2000-01-02
}

// [[Rcpp::export]]
Rcpp::Date years_ago(Rcpp::Date today,
                     int shift){
  Date d(today.getYear()-shift,today.getMonth() ,today.getDay());
  //int ddiff = today-d;
  return d; // 2000-01-02
}

// [[Rcpp::export]]
Rcpp::Date next_year_holiday(Rcpp::Date today,
                             Rcpp::Date this_holiday,
                             Rcpp::Date next_holiday){
  int ddiff = today-this_holiday;
  Date h_date = next_holiday + ddiff;
  return h_date; // 2000-01-02
}

// [[Rcpp::export]]
Rcpp::Date next_year(Rcpp::Date today){
  Date d(today.getYear()+1,today.getMonth() ,today.getDay());
  //int ddiff = today-d;
  return d; // 2000-01-02
}

// [[Rcpp::export]]
arma::uword which_date_leq(Rcpp::Date date,
                           std::vector<Date> Dvec){
  uword j;
  for(j = 0; j < Dvec.size(); j++){
    if(Dvec[j] > date){
      break;
    }
  }
  return(j); //R indexing
}

// [[Rcpp::export]]
arma::uword which_date_geq(Rcpp::Date date,
                           std::vector<Date> Dvec){
  uword j;
  for(j = Dvec.size(); j>0; j--){
    if(Dvec[j-1] < date){
      break;
    }
  }
  return(j+1); //R indexing
}

// [[Rcpp::export]]
arma::uword which_date_closest(Rcpp::Date date,
                               std::vector<Date> Dvec){
  uword j;
  for(j = 0; j < Dvec.size(); j++){
    if(Dvec[j] > date){
      break;
    }
  }
  if(j > 1 && j < Dvec.size()){
    int d_less = date - Dvec[j-1];
    int d_more = Dvec[j] - date;
    if(d_less > d_more){
      j++;
    }
  }
  return(j); //R indexing
}

// [[Rcpp::export]]
arma::uvec which_date_closest_ordered(std::vector<Date> FromVec,    //for each date in this vector
                                      std::vector<Date> IndVec){    //find the index of the closest date in this vector
  int d_more, d_less;
  uword j = 0;
  uword k = 1;
  uvec idx(FromVec.size(), fill::ones);
  idx = idx*IndVec.size();
  while(j<FromVec.size()){
    while(k<IndVec.size()){
      if(FromVec[j] < IndVec[k]){
        d_less = FromVec[j] - IndVec[k-1];
        d_more = IndVec[k] - FromVec[j];
        if(d_more<d_less){
          idx(j) = k + 1; //R indexing
          k++;
          break;
        }else{
          idx(j) = k; //R indexing
          break;
        }
      }else{
        k++;
      }
    }
    j++;
  }
  return(idx);
}

// [[Rcpp::export]]
arma::uword day(Rcpp::Date date){
  uword out = date.getDay();
  return(out);
}

// [[Rcpp::export]]
Rcpp::Date replace_day(Rcpp::Date date,
                        int new_day){
  Date d(date.getYear(),date.getMonth(),new_day);
  return(d);
}


// [[Rcpp::export]]
int MonthDays(double year,
              double month){
  int days;
  if((month == 1) || (month == 3) || (month == 5) || (month == 7) || (month == 8) || (month == 10) || (month == 12)){
    days = 31;
  }
  else if((month == 4) || (month == 6) || (month == 9) || (month == 11) ){
    days = 30;
  }
  else if(round((year-1940)/4) == ((year-1940)/4) ){
    days = 29;
  }
  else{
    days = 28;
  }
  return(days);
}

// // [[Rcpp::export]]
// arma::uvec MonthDaysVec(arma::uvec year,
//                         arma::uvec month){
//   if(year.n_elem !=  month.n_elem{
//     Rcpp::stop("Year and month vectors must be the same size")
//   }
//   uvec days(year.n_elem);
//
//   return(days);
// }


//return last day for the given month
// [[Rcpp::export]]
std::vector<Date> end_of_month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int days;
  if(date.size()>0){
    for(uword j=0; j<date.size(); j++){
      tmp  = date[j];
      days = MonthDays(tmp.getYear(), tmp.getMonth());
      d[j] = Date(tmp.getYear(), tmp.getMonth(), days);
    }
  }
  return(d);
}

// [[Rcpp::export]]
std::vector<Date> end_next_month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int year, month, days;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    month = tmp.getMonth()+1;
    year = tmp.getYear();
    if(month==13){
      month = 1;
      year = year + 1;
    }
    days = MonthDays(year, month);
    d[j] = Date(year, month, days);
  }
  return(d);
}

// [[Rcpp::export]]
std::vector<Date> end_previous_month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int year, month, days;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    month = tmp.getMonth()-1;
    year = tmp.getYear();
    if(month==0){
      month = 12;
      year = year - 1;
    }
    days = MonthDays(year, month);
    d[j] = Date(year, month, days);
  }
  return(d);
}

//return last day for the given month
// [[Rcpp::export]]
Rcpp::Date end_of_month_date(Rcpp::Date date){
  int days = MonthDays(date.getYear(), date.getMonth());
  Rcpp::Date d(date.getYear(), date.getMonth(), days);
  return(d);
}

//return first day for the given month
// [[Rcpp::export]]
std::vector<Date> first_of_month(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    d[j] = Date(tmp.getYear(), tmp.getMonth(), 1);
  }
  return(d);
}

//return last day for the given quarter
// [[Rcpp::export]]
std::vector<Date> end_of_quarter(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int days;
  int mnth;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    mnth = tmp.getMonth();
    if(mnth == 1 || mnth == 2){
      mnth = 3;
    }else if(mnth == 4 || mnth == 5){
      mnth = 6;
    }else if(mnth == 7 || mnth == 8){
      mnth = 9;
    }else if(mnth == 10 || mnth == 11){
      mnth = 12;
    }
    days = MonthDays(tmp.getYear(), mnth);
    d[j] = Date(tmp.getYear(), mnth, days);
  }
  return(d);
}

//return first day for previous quarter
// [[Rcpp::export]]
std::vector<Date> first_previous_quarter(std::vector<Date> date){
  std::vector<Date> d(date.size());
  Rcpp::Date tmp;
  int yr;
  int mnth;
  for(uword j=0; j<date.size(); j++){
    tmp  = date[j];
    mnth = tmp.getMonth();
    yr = tmp.getYear();
    if(mnth == 1 || mnth == 2 || mnth == 3){
      mnth = 10;
      yr = yr-1;
    }else if(mnth == 4 || mnth == 5 || mnth == 6){
      mnth = 1;
    }else if(mnth == 7 || mnth == 8 || mnth == 9){
      mnth = 4;
    }else if(mnth == 10 || mnth == 11 || mnth == 12){
      mnth = 7;
    }
    d[j] = Date(yr, mnth, 1);
  }
  return(d);
}

//return difference in months between two dates
// [[Rcpp::export]]
arma::uvec month_diff(std::vector<Date> first_date,
                                 std::vector<Date> second_date){
  if(first_date.size() != second_date.size()){
    Rcpp::stop("Date vectors must be same length");
  }
  uword T = first_date.size();
  arma::uvec mdiff(T);
  Rcpp::Date tmp1, tmp2;
  for(uword j=0; j<T; j++){
    tmp1  = first_date[j];
    tmp2  = second_date[j];
    mdiff[j] = 12*(tmp1.getYear()-tmp2.getYear()) + tmp1.getMonth() -  tmp2.getMonth();
  }
  return(mdiff);
}

// [[Rcpp::export]]
arma::uword ps_week(Rcpp::Date day){
  uword d = day.getDay();
  int tmp = floor(d/7) - 1;
  int zip = 0;
  uword week = std::max(zip,tmp);
  return(week);
}

// [[Rcpp::export]]
std::vector<Date> pseudo_weekly_sequence(Rcpp::Date start,
                                        arma::uword length = 0,
                                        Rcpp::Date end = 0){

  uword year, month, week;
  uvec days;
  days << 7 << 14 << 21 << 28 << endr;
  if(length == 0){
    if(end-start<0){
      Rcpp::stop("'end' must come after 'start");
    }
    year = end.getYear() - start.getYear();
    month = end.getMonth() - start.getMonth();
    week = ps_week(end) - ps_week(start);
    length = 48*year + 4*month + week + 1; //+1 bc week returns the index value
  }
  year = start.getYear();
  month = start.getMonth();
  week = ps_week(start);
  std::vector<Date> out(length);
  for(uword j = 0; j<length; j++){
    if(week<3){
      out[j] = Date(year,month,days[week]);
    }else{
      out[j] = end_of_month_date(Date(year,month,days[week]));
    }
    week++;
    if(week>3){
      week = 0;
      month++;
    }
    if(month>12){
      month = 1;
      year++;
    }
  }
  return(out);
}

//std::vector<Date>
// [[Rcpp::export]]
Rcpp::Date pseudo_weekly_date(Rcpp::Date date){
  double tmp = date.getDay();
  int week = std::ceil(tmp/7);
  int day;
  if(week==4 || week == 5){
    day = MonthDays(date.getYear(), date.getMonth());
  }else{
    day = week*7;
  }
  Rcpp::Date d(date.getYear(), date.getMonth(), day);
  return(d);
}

// [[Rcpp::export]]
Rcpp::Date numeric_to_date(int year,
                           int month,
                           int day){
  Rcpp::Date d(year, month, day);
  return(d);
}

// std::vector<Date> d(date.size());
// Rcpp::Date tmp;
// int day;
// double td;
// int week;
// for(uword j=0; j<date.size(); j++){
//   tmp  = date[j];
//   td = tmp.getDay()/7;
//   week = std::ceil(td);
//   if(week==4 || week == 5){
//     day = MonthDays(tmp.getYear(), tmp.getMonth());
//   }else{
//     day = week*7;
//   }
//   d[j] = Date(tmp.getYear(), tmp.getMonth(), day);

// [[Rcpp::export]]
List SARMA(arma::vec Y, //univariate data
            arma::rowvec p, //AR parameters
            arma::rowvec q, //MA parameters
            arma::rowvec P, //seasonal AR parameters
            arma::rowvec Q, //seasonal MA parameters
            arma::umat P_lag, //seasonal AR lags. field format allows different lenghts at each date to account for holiday effects
            arma::umat Q_lag){ //seasonal MA lags

  // first "throw away" observation is a zero.
  Y = join_vert(zeros<vec>(1),Y);

  uword T   = Y.n_rows;
  uword T_long = std::max(Q_lag.n_cols,P_lag.n_cols);
  T_long = std::max(T,T_long+1);
  uword sp  = p.n_cols;
  uword sq  = q.n_cols;
  uword pq  = std::max(sp,sq);
  double s_AR, s_MA;
  vec E(T, fill::zeros);
  //vec eps(T_long, fill::zeros);
  vec seas(T_long, fill::zeros);
  vec YP(T, fill::zeros);
  vec Y_sa(T, fill::zeros);
  uvec all_ind;
  //where to start the iterations
  // uvec Pl(2, fill::zeros);
  // uvec Ql(2, fill::zeros);
  // if(P.n_cols>0){
  //   Pl = ind2sub(size(P_lag), max(find(P_lag==0)));
  // }
  // if(Q.n_cols>0){
  //   Ql = ind2sub(size(Q_lag), max(find(Q_lag==0)));
  // }
  // uword srt = std::max(pq + Pl[0], pq + Ql[0]);

  if(sp==0 && sq==0){
    Rcpp::stop("Seasonal adjustment requires at least 1 AR lag");
  }else if(sp>0 && sq==0){
    if(P.n_cols==0 && Q.n_cols==0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y(span(t-sp,t-1)))); //predicting next period SA values
        if(!is_finite(YP(t))) YP(t) = as_scalar(p*flipud(YP(span(t-sp,t-1))));
        E(t) = Y(t) - YP(t);
        if(!is_finite(E(t))) E(t) = 0;
      }
    }else if(P.n_cols>0 && Q.n_cols==0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))); //predicting next period SA values
        //update
        s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
        if(!is_finite(s_AR)) s_AR = as_scalar(P*(YP(P_lag.row(t-1)) + seas(P_lag.row(t-1)) )); //Y(span(t-pq,t)).is_finite() &&
        seas(t) = s_AR; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }else if(P.n_cols==0 && Q.n_cols>0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))); //predicting next period SA values
        //update
        s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
        seas(t) = s_MA; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }else{
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))); //predicting next period SA values
        //update
        s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
        s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
        if(!is_finite(s_AR)) s_AR = as_scalar(P*(YP(P_lag.row(t-1)) + seas(P_lag.row(t-1)) )); //Y(span(t-pq,t)).is_finite() &&
        seas(t) = s_AR + s_MA; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }
  }else if(sp==0 && sq>0){ //if sp>0 and sq==0
    Rcpp::stop("Seasonal adjustment requires at least 1 AR lag");
  }else{
    if(P.n_cols==0 && Q.n_cols==0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y(span(t-sp,t-1)))) + as_scalar(q*flipud(E(span(t-sq,t-1)))); //predicting next period SA values
        if(!is_finite(YP(t))) YP(t) = as_scalar(p*flipud(YP(span(t-sp,t-1)))) + as_scalar(q*flipud(E(span(t-sq,t-1))));
        E(t) = Y(t) - YP(t);
        if(!is_finite(E(t))) E(t) = 0;
      }
    }else if(P.n_cols>0 && Q.n_cols==0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))) + as_scalar(q*flipud(E(span(t-sq,t-1)))); //predicting next period SA values
        //update
        s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
        if(!is_finite(s_AR)) s_AR = as_scalar(P*(YP(P_lag.row(t-1)) + seas(P_lag.row(t-1)) )); //Y(span(t-pq,t)).is_finite() &&
        seas(t) = s_AR; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }else if(P.n_cols==0 && Q.n_cols>0){
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))) + as_scalar(q*flipud(E(span(t-sq,t-1)))); //predicting next period SA values
        //update
        s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
        seas(t) = s_MA; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }else{
      for(uword t = pq+1; t<T; t++){
        //predict
        YP(t) = as_scalar(p*flipud(Y_sa(span(t-sp,t-1)))) + as_scalar(q*flipud(E(span(t-sq,t-1)))); //predicting next period SA values
        //update
        s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
        s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
        if(!is_finite(s_AR)) s_AR = as_scalar(P*(YP(P_lag.row(t-1)) + seas(P_lag.row(t-1)) )); //Y(span(t-pq,t)).is_finite() &&
        seas(t) = s_AR + s_MA; //seasonal component
        Y_sa(t) = Y(t) - seas(t);
        if(!is_finite(Y_sa(t))) Y_sa(t) = YP(t);
        E(t) = Y_sa(t) - YP(t);
      }
    }
  }

  // //Get future seasonal adjustments if required
  // if(T_long > T){
  //   if(P.n_cols==0 && Q.n_cols>0){
  //     for(uword t = T; t<T_long; t++){
  //       s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
  //       seas(t) = s_MA; //seasonal component
  //     }
  //   }else if(P.n_cols>0 && Q.n_cols==0){
  //     for(uword t = T; t<T_long; t++){
  //       s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
  //       if( !is_finite(s_AR) ) continue;
  //       seas(t) = s_AR; //seasonal component
  //     }
  //   }else{
  //     for(uword t = T; t<T_long; t++){
  //       s_MA = as_scalar(Q*Y_sa(Q_lag.row(t-1))); //seasonal MA covariates
  //       s_AR = as_scalar(P*Y(P_lag.row(t-1))); //seasonal AR covariates
  //       if( !is_finite(s_AR) ) s_AR = 0;
  //       seas(t) = s_AR + s_MA;
  //     }
  //   }
  // } // if(T_long > T)

  //double MSE = as_scalar(trans(E(span(srt,T-1)))*E(span(srt,T-1)))/T;

  seas.shed_row(0);
  Y_sa.shed_row(0);
  E.shed_row(0);
  YP.shed_row(0);

  uvec ind = find(E); //find non-zero elements of E
  double MSE = as_scalar(trans(E(ind))*E(ind))/(ind.n_elem);


  List Out;
  Out["MSE"] = MSE;
  Out["seas"]  = seas;
  Out["Y_sa"] = Y_sa;
  Out["E"] = E;
  Out["YP"] = YP;
  Out["p"]  = p;
  Out["q"] = q;
  Out["P"] = P;
  Out["Q"] = Q;

  return(Out);
}