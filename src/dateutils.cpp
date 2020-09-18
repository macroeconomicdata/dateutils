// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;


//Stack times series data in VAR format
// [[Rcpp::export]]
arma:: mat Stack_Obs(arma::mat nn, arma::uword p, arma::uword r = 0){
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
arma::uword Day(Rcpp::Date date){
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
std::vector<Date> End_of_Month(std::vector<Date> date){
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
std::vector<Date> End_next_Month(std::vector<Date> date){
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
std::vector<Date> End_previous_Month(std::vector<Date> date){
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

//return last day for the given quarter
// [[Rcpp::export]]
std::vector<Date> End_of_Quarter(std::vector<Date> date){
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
std::vector<Date> First_previous_Quarter(std::vector<Date> date){
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
