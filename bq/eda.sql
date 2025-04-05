-- median of clv by marital_status
select
distinct marital_status
,median_clv
from (
  select
  marital_status
  ,percentile_cont(clv, 0.5) over (partition by marital_status) as median_clv
from `airline.loyalty_history`)
order by 2

--average clv by city
select 
avg(clv) as average_clv
,city
from `airline.loyalty_history`
group by city
order by average_clv desc;

--average clv by marital status
select 
avg(clv) as average_clv
,avg(salary) as average_salary
,marital_status
from `airline.loyalty_history`
group by
marital_status
order by average_clv desc;

select
count(marital_status) as total_divorced
from `airline.loyalty_history`
where marital_status = 'Divorced';
--2518

select
count(marital_status) as total_married
from `airline.loyalty_history`
where marital_status = 'Married';
--9735

select
count(marital_status) as total_single
from `airline.loyalty_history`
where marital_status = 'Single';
--4484

--what are the enrollment and cancellation years? cancellation must be > enrollment, unless cancellation = 0; meaning they have not cancelled
select
enrollment_year
,cancellation_year
from `airline.loyalty_history`
group by 1, 2
order by 2 asc;

--checking unique values for loyalty number, salary, clv
select
loyalty_number
,salary
,clv
from `airline.loyalty_history`
group by 1,2, 3
order by 2 asc;

--counting how many salaries are negative (this is so I can clean in r)
select 
count(distinct case when salary < 0 then loyalty_number end) as broke_people
from `airline.loyalty_history`;

-- JOIN -> removal of int64_field_0, int64_field_0_1 and loyalty_number_1 done in R
--file name = all_together_now
SELECT *,
 FORMAT("%04d-%02d", Enrollment_Month, Enrollment_Year) AS Enrollment_Date,
  CASE
    WHEN Cancellation_Year IS NULL OR Cancellation_Year = 0 
      OR Cancellation_Month IS NULL OR Cancellation_Month =0 
      THEN 'Not Cancelled'
    ELSE CONCAT(
      (Cancellation_Year - Enrollment_Year), ' years, ',
      ((Cancellation_Year - Enrollment_Year) * 12 + (Cancellation_Month - Enrollment_Month)), ' months'
    )
  END AS Enrollment_Period,
FROM `airline.flight_activity` AS Flight_Activity
JOIN `airline.loyalty_history` AS Loyalty_History 
  ON Flight_Activity.Loyalty_Number = Loyalty_History.Loyalty_number

-- next steps from zero
-- uuuhh so far in my head i like have it set up that i have a client and so im summarizing the data and where the program is now then using ML to  able to predict the CLV and potentially enrollment period of customers based off of the different factors in the data??
-- I have a strong feeling i can predict enrollment and cancellation peaks by visualizing the data (not to mention its only 2 years long anyhow)
