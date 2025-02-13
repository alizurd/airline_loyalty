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
