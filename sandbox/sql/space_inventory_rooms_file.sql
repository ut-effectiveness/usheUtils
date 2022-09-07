--Space Inventory Rooms
SELECT --a.building_id,
       b.building_number,
       a.room_number,
       '' AS room_suffix,
       a.room_group1_code,
       a.room_use_code,
       a.room_name,
       a.room_stations,
       a.room_area,
       a.room_disabled_access,
       a.room_prorated,
       a.room_prorated_area,
       a.room_activity_date
FROM export.rooms a
LEFT JOIN export.buildings b
ON a.building_id = b.buildings_id
WHERE is_state_reported
 AND (building_to_term_id IS NULL OR building_to_term_id > (SELECT DISTINCT term_id FROM quad.term WHERE is_current_term))
 AND building_from_term_id <= (SELECT DISTINCT term_id FROM quad.term WHERE is_current_term);


