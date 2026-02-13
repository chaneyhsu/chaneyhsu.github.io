CS779_PROJECT.PUBLIC.RAW_311_STAGECS779_PROJECTCREATE DATABASE CS779_PROJECT;

SELECT COUNT(*) FROM RAW_311_JSON;

SELECT RAW FROM RAW_311_JSON LIMIT 5;

-- RAW:key /  extracts a field from the JSON object.

-- RAW:key::string / casts the JSON value into a regular SQL type

SELECT
    RAW:case_id::string AS case_id,
    RAW:service_type::string AS service_type,
    RAW:service_name::string AS service_name,
    RAW:zipcode::string AS zipcode,
    RAW:department_name::string AS department_name,
    RAW:resolution_days::float AS resolution_days,
    RAW:on_time_flag::string AS on_time_flag,
    RAW:request_open_date::date AS request_open_date
FROM RAW_311_JSON
LIMIT 20;


-- 1. Which service types occur most frequently each month?
WITH MonthlyCounts AS (
    SELECT
        YEAR(TO_DATE(RAW:request_open_date)) AS year,
        MONTH(TO_DATE(RAW:request_open_date)) AS month,
        RAW:service_type::string AS service_type,
        COUNT(*) AS request_count,
        ROW_NUMBER() OVER (
            PARTITION BY YEAR(TO_DATE(RAW:request_open_date)),
                         MONTH(TO_DATE(RAW:request_open_date))
            ORDER BY COUNT(*) DESC
        ) AS rn
    FROM RAW_311_JSON
    WHERE RAW:request_open_date IS NOT NULL
      AND RAW:service_type IS NOT NULL
    GROUP BY year, month, service_type
)
SELECT 
    year,
    month,
    service_type,
    request_count
FROM MonthlyCounts
WHERE rn = 1
ORDER BY year, month;

-- 2. Which zipcodes generate the highest volume of requests?
SELECT 
    RAW:zipcode::string AS zipcode,
    COUNT(*) AS request_count
FROM RAW_311_JSON
WHERE RAW:zipcode IS NOT NULL
GROUP BY zipcode
ORDER BY request_count DESC;


-- 3. Which departments have the shortest avg resolution time?

SELECT 
    RAW:department_name::string AS department_name,
    ROUND(AVG(RAW:resolution_days::float), 2) AS avg_resolution_days
FROM RAW_311_JSON
WHERE RAW:department_name IS NOT NULL
  AND RAW:resolution_days IS NOT NULL
GROUP BY department_name
ORDER BY avg_resolution_days ASC;


-- 4. Compare avg resolution time & on-time performance for each service type
SELECT
    RAW:service_type::string AS service_type,
    
    ROUND(AVG(RAW:resolution_days::float), 2) AS avg_resolution_days,

    ROUND(AVG(CASE WHEN RAW:on_time_flag::string = 'TRUE' THEN 1 ELSE 0 END), 3
    ) AS on_time_rate,

    COUNT(*) AS total_requests

FROM RAW_311_JSON
WHERE RAW:service_type IS NOT NULL
GROUP BY service_type
ORDER BY avg_resolution_days ASC;

