-- select *
-- from `airline.history` h
-- left join `airline.flights` f
-- on h.loyalty_number = f.loyalty_number;

select 
loyalty_number
,year
,month
,sum(total_flights) as flights
,sum(distance)
,sum(points_accumulated) as sum_points_accumulated
,sum(points_redeemed) as sum_points_redeemed
,sum(dollar_cost_points_redeemed) as sum_dollar_cost_points_redeemed
from `airline.flights`
where loyalty_number = 399917
group by 1, 2, 3


