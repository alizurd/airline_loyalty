--average clv by city
select 
avg(clv) as average_clv
,city
from `airline.loyalty_history`
group by city
order by average_clv desc;
