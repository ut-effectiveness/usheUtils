--Space Inventory Rooms
SELECT a.building_id,
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
       a.room_acitivity_date
FROM export.rooms a;


