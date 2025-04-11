# airline loyalty churn project

The primary purpose of this project is to determine what causes customers to churn out of a *fictional* airlines loyalty program. secondary analyses include uncovering predictors of clv (customer lifetime value) and relationships between variables in the data.

## flights file
| Column        | Description   |
| ------------- | ------------- |
| `loyalty_number`  |  Customer's unique loyalty number |
| `year`  | Year of the period  |
| `month`  | Month of the period |
| `total_flights`  | Sum of Flights Booked (all tickets purchased in the period) |
| `distance`  | Flight distance traveled in the period (km)  |
| `points_accumulated`  | Loyalty points accumulated in the period  |
| `points_redeemed`  | Loyalty points redeemed in the period |
| `dollar_cost_points_redeemed`  | Dollar equivalent for points redeemed in the period in CDN |

## history file
| Column        | Description   |
| ------------- | ------------- |
| `loyalty_number`  | Customer's unique loyalty number |
| `country`  | Country of residence  |
| `province`  | Province of residence |
| `city`  | City of residence |
| `postal_code`  | Postal code of residence  |
| `gender`  | Gender  |
| `education`  | Highest education level (High school or lower > College > Bachelor > Master > Doctor) |
| `salary`  | Annual income  |
| `marital_status`  | Marital status (Single, Married, Divorced) |
| `loyalty_card`  | Loyalty card status (Star > Nova > Aurora)  |
| `clv`  | Customer lifetime value - total invoice value for all flights ever booked by member |
| `enrollment_type`  | Enrollment type (Standard / 2018 Promotion)  |
| `enrollment_year` | Year Member enrolled in membership program |
| `enrollment_month`  | Month Member enrolled in membership program  |
| `cancellation_year`  | Year Member cancelled their membership  |
| `cancellation_month`  | Month Member cancelled their membership |

## TODO

Status: we've created a base model and are continuing to refine it, as well as EDA to derive interesting insights! more updates to come~~