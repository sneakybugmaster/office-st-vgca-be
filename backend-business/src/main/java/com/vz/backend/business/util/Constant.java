package com.vz.backend.business.util;

/**
 * @author DucND
 * @date 28 thg 7, 2020
 */
public class Constant {

	public static final int TASK_STATUS_NEW = 0;
	public static final int TASK_STATUS_INPROCESS = 1;
	public static final int TASK_STATUS_REJECT = 2;
	public static final int TASK_STATUS_COMPLETE = 3;
	public static final int TASK_STATUS_CLOSE = 4;
	public static final int TASK_STATUS_REVOKE = 5;

	public static final int TASK_APPROVE_WAIT = 0;
	public static final int TASK_APPROVE_ACCEPT = 1;
	public static final int TASK_APPROVE_REJECT = 2;

	public static final int TASK_EXCUTE_STATUS_NEW = 0;
	public static final int TASK_EXCUTE_STATUS_INPROCESS = 1;
	public static final int TASK_EXCUTE_STATUS_REJECT = 2;
	public static final int TASK_EXCUTE_STATUS_COMPLETE = 3;
	public static final int TASK_EXCUTE_STATUS_CLOSE = 4;
	public static final int TASK_EXCUTE_STATUS_REVOKE = 5;
	public static final Long TYPE_TASK_REFUSE = 6L;
	
	//for quick search all object
	public static final String UNION_ALL = " UNION ALL ";
	public static final String QUICK_SEARCH_QUERY = 
			" SELECT d.id, d.number_sign as code, d.preview as name, d.create_date,  'Văn bản đến' as type "
			+ " FROM vz.DOCUMENT d "
			+ " LEFT JOIN vz.SYS_DOCUMENT_IN_PROCESS p on p.doc_id = d.id AND p.active = TRUE AND p.client_id = d.client_id "
			+ " LEFT JOIN vz.DOCUMENT_USER du ON du.doc_id = d.id AND du.user_id = :userId AND du.doc_type = 'VAN_BAN_DEN' AND d.client_id = du.client_id "
			+ " LEFT JOIN vz.CLERICAL_ORG c ON :clericalOrg IS TRUE AND c.user_id = :userId AND c.active = TRUE AND d.client_id = c.client_id "
			+ " LEFT JOIN vz.SYS_CATEGORY sc ON d.doctype_id = sc.id AND sc.active = TRUE AND d.client_id = sc.client_id "
			+ " LEFT JOIN vz.DOCUMENT_BOOK db ON d.book_id = db.id AND db.active = TRUE AND d.client_id = db.client_id "
			+ " LEFT JOIN vz.OBJECT_TAG ot ON ot.obj_id = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_DEN' "
			+ " LEFT JOIN vz.Tag t ON t.id = ot.tag_id AND t.active = TRUE AND t.create_by = :userId "
			+ " WHERE ((:text IS NULL) OR LOWER(d.number_sign) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(d.preview) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ "	OR LOWER(d.number_arrival_str) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR LOWER(db.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR LOWER(sc.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR LOWER(t.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR :text LIKE 'văn bản đến') "
			+ " AND (:clientId IS NULL OR d.client_id = :clientId ) AND d.active = TRUE "
			+ " AND d.status != 'RETAKE_DOC'"
			+ " AND ((p.doc_id, COALESCE(p.update_date, p.create_date)) IN (SELECT sp3.doc_id , MAX(COALESCE(sp3.update_date, sp3.create_date)) FROM vz.SYS_DOCUMENT_IN_PROCESS sp3 WHERE sp3.active = TRUE AND sp3.client_id = :clientId AND (sp3.to_user IN (:userIds) OR sp3.delegater_id IN (:userIds)) GROUP BY sp3.doc_id )"
			+ " OR d.id IN (SELECT sd.id FROM vz.DOCUMENT sd WHERE sd.client_id = :clientId AND sd.active = TRUE "
			+ " AND ((:clericalOrg IS TRUE AND sd.org_receive = c.org_id) OR (:clericalOrg IS FALSE AND sd.person_enter_id IN (:userIds))) "
			+ " AND :isLibrarianDocIn = true AND sd.status = 'NOT_YET'))"
			+ " GROUP BY d.id, d.number_sign, d.preview, d.create_date "
			+   UNION_ALL
			+ " SELECT d.id, d.number_sign as code, d.preview as name, d.create_date,  'Văn bản đi' as type "
			+ " FROM vz.DOCUMENT_OUT d "
			+ "	LEFT JOIN vz.DOCUMENT_USER du ON du.doc_id = d.id AND du.user_id = :userId AND du.doc_type = 'VAN_BAN_DI' AND d.client_id = du.client_id "
			+ " LEFT JOIN vz.CLERICAL_ORG c ON :clericalOrg IS TRUE AND c.user_id = :userId AND c.active IS TRUE AND d.client_id = c.client_id "
			+ " LEFT JOIN vz.SYS_CATEGORY sc ON d.doctype_id = sc.id AND sc.active = TRUE AND d.client_id = sc.client_id "
			+ " LEFT JOIN vz.DOCUMENT_BOOK db ON d.book_id = db.id AND db.active = TRUE AND d.client_id = db.client_id "
			+ " LEFT JOIN vz.OBJECT_TAG ot ON ot.obj_id = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_DI' "
			+ " LEFT JOIN vz.Tag t ON t.id = ot.tag_id AND t.active = TRUE AND t.create_by = :userId "
			+ " WHERE d.active = TRUE "
			+ " AND (:text IS NULL OR LOWER(d.number_sign) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(d.preview) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(d.outside_receive) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ "	OR LOWER(db.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR LOWER(sc.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ "	OR LOWER(t.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ "	OR :text LIKE 'văn bản đi') "
			+ " AND d.client_id = :clientId AND d.status != 'THU_HOI_BH' AND ("
			+ " d.id IN (SELECT sd.id FROM vz.DOCUMENT_OUT sd JOIN vz.SYS_DOCUMENT_OUT_PROCESS p ON sd.id = p.doc_id "
			+ " 		LEFT JOIN vz.DELEGATE d1 ON d1.id = p.delegate_id AND d1.start_date <= :date AND d1.end_date >= :date "
			+ " 		LEFT JOIN vz.SYS_USER u2 ON u2.id = d1.to_user "
			+ " 		LEFT JOIN vz.SYS_USER u1 ON u1.id = p.delegate_user_id "
			+ " 		WHERE (p.user_id = :userId OR d1.to_user = :userId) AND p.active = TRUE AND p.client_id = :clientId AND sd.client_id=:clientId)"
			+ " OR d.id IN (SELECT dr.doc_id FROM vz.DOCUMENT_RECEIVE dr WHERE dr.receive_id = :userId AND dr.type = 'USER' AND dr.active = TRUE AND dr.client_id = :clientId)"
			+ " OR ((:lead = TRUE) AND d.id IN (SELECT dr2.doc_id FROM vz.DOCUMENT_RECEIVE dr2 WHERE dr2.receive_id = :orgId AND dr2.type = 'ORG' AND dr2.active = TRUE AND dr2.client_id=:clientId))"
			+ " OR (d.id IN (SELECT do0.id FROM vz.DOCUMENT_OUT do0 WHERE do0.create_by = (:userId) AND do0.active = TRUE AND do0.client_id =:clientId))"
			+ " OR ((:isLibrarianDocOut = TRUE) AND d.id IN (SELECT do1.id FROM vz.DOCUMENT_OUT do1 WHERE (do1.status = 'CHO_BAN_HANH' OR do1.status = 'DA_BAN_HANH') AND do1.active = TRUE AND do1.client_id = :clientId "
			+ " AND ((:clericalOrg IS TRUE AND do1.org_issued_id = c.org_id) OR (:clericalOrg IS false AND do1.org_issued_id = :orgId)))) "
			+ " )"
			+ " GROUP BY d.id, d.number_sign, d.preview, d.create_date "
			+   UNION_ALL
			+ " SELECT t.id, t.code_task as code, t.task_name as name, t.create_date, 'Công việc' as type "
			+ " FROM vz.TASK t LEFT JOIN vz.TASK_EXCUTE tx ON tx.task_id = t.id "
			+ " LEFT JOIN vz.OBJECT_TAG ot ON ot.obj_id = t.id AND ot.active = TRUE AND ot.type = 'GIAO_VIEC' "
			+ " LEFT JOIN vz.Tag tg ON t.id = ot.tag_id AND tg.active = TRUE AND tg.create_by = :userId "
			+ " WHERE t.active = TRUE AND t.client_id = :clientId "
			+ " AND tx.client_id = :clientId AND tx.active = TRUE"
			+ " AND (t.user_assign_id = :userId OR t.user_ext_id =:userId OR tx.user_id =:userId)" 
			+ " AND (:text IS NULL OR LOWER(t.task_name) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ " OR LOWER(t.code_task) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ "	OR LOWER(tg.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ " OR :text LIKE 'công việc')"
			+ " GROUP BY t.id, t.code_task, t.task_name, t.create_date "
			+   UNION_ALL
			+ " SELECT d1.id, d1.number_sign as code, d1.preview, d1.create_date, 'Văn bản nội bộ' as type "
			+ " FROM vz.DOCUMENT_INTERNAL d1 WHERE d1.id IN (SELECT d.id "
			+ " FROM vz.DOCUMENT_INTERNAL d LEFT JOIN vz.DOC_INTERNAL_APPROVE a ON a.doc_id = d.id AND a.active IS TRUE "
			+ " INNER JOIN vz.SYS_USER u ON u.id = :userId "
			+ " INNER JOIN vz.SYS_CATEGORY c ON c.id = u.position_id "
			+ " LEFT JOIN vz.OBJECT_TAG ot ON ot.obj_id = d1.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_NOI_BO' "
			+ " LEFT JOIN vz.Tag t ON t.id = ot.tag_id AND t.active = TRUE AND t.create_by = :userId "
			+ " WHERE d.active IS TRUE AND d.client_id = :clientId "
			+ " AND (d.create_by = :userId OR (a.type != 'SIGNER' AND (a.user_id = :userId OR (a.org_id = u.org_id AND c.is_leadership IS TRUE))) "
			+ " OR (a.type = 'SIGNER' AND a.user_id = :userId AND d.status IN ('NB_LANH_DAO_KY', 'NB_BAN_HANH'))) "
			+ " AND (:text IS NULL OR LOWER(d.number_sign) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ " OR LOWER(d.preview) LIKE CONCAT('%', CAST(:text AS TEXT), '%') "
			+ "	OR LOWER(t.name) LIKE CONCAT('%', CAST(:text AS TEXT), '%')"
			+ " OR :text LIKE 'văn bản nội bộ') "
			+ " )"
			+   UNION_ALL
			+ " SELECT c.id, c.address, c.title, c.create_date, 'Lịch họp' as type FROM vz.CALENDAR2 c "
			+ " LEFT JOIN vz.CALENDAR_JOIN2 j ON j.calendar_id = c.id " 
			+ " WHERE c.client_id = :clientId AND j.client_id = :clientId" 
			+ " AND c.active IS TRUE AND j.active IS TRUE"
			+ " AND (:userId IS NULL OR j.user_id = :userId OR c.create_by = :userId)"
			+ " AND c.is_meeting_calendar IS TRUE"
			+ " AND (:text IS NULL OR LOWER(c.address) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(c.title) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR :text LIKE 'lịch họp') "
//			+ " AND c.start_time > NOW() "
			+ " GROUP BY c.id, c.address, c.title, c.create_date "
			+ UNION_ALL
			+ " SELECT c.id, c.address, c.title, c.create_date, 'Lịch đơn vị' as type FROM vz.CALENDAR2 c "
			+ " LEFT JOIN vz.SYS_ORGANIZATION o ON c.org_id = o.id "
			+ " WHERE c.client_id = :clientId AND c.active = TRUE"
//			+ " AND (c.start_time BETWEEN :startDate AND :endDate OR c.end_time BETWEEN :startDate AND :endDate)"
			+ " AND c.org_id in (:orgIdUnitLvs)" 
			+ " AND c.register_ban is FALSE  AND o.name != 'Ban'"
			+ " AND c.is_meeting_calendar IS FALSE"
			+ " AND (:text IS NULL OR LOWER(c.address) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(c.title) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR :text LIKE 'lịch công tác đơn vị') "
			+ UNION_ALL
			+ " SELECT c.id, c.address, c.title, c.create_date, 'Lịch Ban' as type FROM vz.CALENDAR2 c "
			+ " LEFT JOIN vz.SYS_ORGANIZATION o ON c.org_id = o.id "
			+ " WHERE c.client_id = :clientId AND c.active = TRUE"
//			+ " AND (c.start_time BETWEEN :startDate AND :endDate OR c.end_time BETWEEN :startDate AND :endDate)"
			+ " AND ((o.name = 'Ban' AND c.org_id = :orgTopLv) OR (o.name != 'Ban' AND c.register_ban is TRUE AND c.org_id in (:orgIdTopLvs))) "
			+ " AND c.is_meeting_calendar IS FALSE"
			+ " AND (:text IS NULL OR LOWER(c.address) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR LOWER(c.title) LIKE CONCAT('%', CAST(:text AS TEXT), '%') OR :text LIKE 'lịch công tác ban') "
			+ " ORDER BY create_date DESC";
	
	public static final String DOC_IN_MENU_QUERY = 
			" SELECT COUNT(DISTINCT p.doc_id) AS count, CONCAT(p.handle_type, '') AS status, d.merged_lines AS mergedLines"
			+ " FROM vz.SYS_DOCUMENT_IN_PROCESS p "
			+ " INNER JOIN vz.DOCUMENT d ON p.doc_id = d.id AND p.active = TRUE AND p.client_id = d.client_id "
			+ " WHERE (:clientId IS NULL OR d.client_id = :clientId ) AND d.active = TRUE "
			+ " AND (p.to_user =:userId OR p.delegater_id = :userId)"
			+ " AND d.status IN ('DOING', 'RETURN_DOC', 'DONE')"
			+ " AND p.handle_status IN ('CHO_XU_LY', 'DANG_XU_LY','CHUYEN_DON_VI', 'CHO_DANH_GIA', 'XIN_DANH_GIA', 'DG_CHAP_NHAN', 'DG_TU_CHOI', 'DA_XU_LY_ADD_USER', 'THU_HOI_HOAN_THANH') "
			+ " AND (p.doc_id, p.step) IN (SELECT sp.doc_id , MAX(sp.step) FROM vz.SYS_DOCUMENT_IN_PROCESS sp WHERE sp.client_id = :clientId AND (sp.to_user = :userId OR sp.delegater_id = :userId ) AND sp.active = TRUE GROUP BY sp.doc_id) "
			+ " GROUP BY p.handle_type,  d.merged_lines"
			+ UNION_ALL
			+ " SELECT COUNT(1) AS count, 'CHO_CHO_Y_KIEN' AS status, 'FALSE' AS mergedLines FROM vz.SYS_DOCUMENT_IN_MANIPULATION m "
			+ " INNER JOIN vz.DOCUMENT d ON m.doc_id = d.id and m.active = TRUE and m.client_id = d.client_id "
			+ " WHERE m.to_user = :userId AND m.handle_status = 'CHO_CHO_Y_KIEN' "
			+ " AND m.active = TRUE AND m.client_id = :clientId "
			+ " AND d.status IN ('DOING', 'RETURN_DOC', 'DONE')"
			+ UNION_ALL
			+ " SELECT COUNT(d) AS count, 'WAIT_RECEIVE' AS status , 'FALSE' AS mergedLines "
			+ " FROM vz.DOCUMENT d "
			+ " LEFT JOIN vz.CLERICAL_ORG c ON :clericalOrg IS TRUE AND c.user_id = :userId AND c.active = TRUE AND c.client_id = :clientId"
			+ " WHERE d.client_id = :clientId  AND d.active = TRUE AND d.status = 'WAIT_RECEIVE'"
			+ " AND ((:clericalOrg IS TRUE AND d.org_receive = c.org_id) OR (:clericalOrg IS FALSE AND d.org_receive = :orgId)) "
			;
	public static final String DOC_OUT_MENU_QUERY =
			" SELECT COUNT(1) AS count, 'DRAFT_HANDLE' AS status "
			+ " FROM vz.SYS_DOCUMENT_OUT_PROCESS p "
			+ " INNER JOIN vz.DOCUMENT_OUT d ON p.doc_id = d.id AND p.active = TRUE AND p.client_id = d.client_id "
			+ " LEFT JOIN vz.DELEGATE d1 ON d1.id = p.delegate_id AND d1.start_date <= :date AND d1.end_date >= :date "
			+ " LEFT JOIN vz.SYS_USER u2 ON u2.id = d1.to_user "
			+ " LEFT JOIN vz.SYS_USER u1 ON u1.id = p.delegate_user_id "
			+ " WHERE d.client_id = :clientId AND d.active = TRUE "
			+ " AND (p.doc_id, COALESCE(p.update_date, p.create_date)) IN (SELECT sp.doc_id , MAX(COALESCE(sp.update_date, sp.create_date)) FROM vz.SYS_DOCUMENT_OUT_PROCESS sp WHERE sp.client_id = :clientId AND (sp.user_id = :userId OR d1.to_user = :userId) AND sp.active = TRUE GROUP BY sp.doc_id) "
			+ " AND p.handle_status IN ('CHO_XU_LY', 'BI_TRA_LAI', 'CHO_Y_KIEN') "
			+ " AND d.status IN ('DU_THAO', 'BI_TRA_LAI', 'DANG_XU_LY', 'THU_HOI_XL', 'CHO_BAN_HANH', 'DA_BAN_HANH') "
			+ " AND (p.user_id = :userId OR d1.to_user = :userId)"
			+ UNION_ALL
			+ " SELECT COUNT(d.id) AS count, 'DU_THAO' AS status "
			+ " FROM vz.DOCUMENT_OUT d "
			+ " LEFT JOIN vz.SYS_DOCUMENT_OUT_PROCESS p ON p.doc_id = d.id "
			+ " WHERE d.client_id = :clientId AND d.active = TRUE "
			+ " AND (d.person_enter_id =:userId)"
			+ " AND (p IS NULL OR p.handle_status = 'DU_THAO')"
			+ " AND (p IS NULL OR p.active = TRUE)"
			+ " AND d.status IN ('DU_THAO', 'BI_TRA_LAI', 'THU_HOI_XL')"
			+ UNION_ALL
			+ "SELECT COUNT(d.id) AS count, 'DRAFT_ISSUED' AS status FROM vz.DOCUMENT_OUT d "
			+ "LEFT JOIN vz.CLERICAL_ORG c ON :clericalOrg IS TRUE AND c.user_id = :userId AND c.active IS TRUE "
			+ "WHERE d.client_id = :clientId AND d.active=TRUE AND d.status IN ('CHO_BAN_HANH')"
			+ "AND ((:clericalOrg IS TRUE AND d.org_issued_id = c.org_id) OR (:clericalOrg IS FALSE AND d.org_issued_id = :orgId)) "
			+ UNION_ALL
			+ "SELECT COUNT(d.id) AS count, 'DOCUMENT_IN_LIST' AS status "
			+ "FROM vz.DOCUMENT_OUT d "
			+ "LEFT JOIN vz.SYS_USER u3 ON u3.id = :userId "
			+ "LEFT JOIN vz.OBJECT_READ obr ON obr.obj_id =  d.id AND obr.user_id = :userId AND obr.type = 'VAN_BAN_DI' "
			+ "LEFT JOIN vz.DOCUMENT_RECEIVE dr ON dr.doc_id = d.id AND dr.active IS TRUE AND dr.receive_id = :userId "
			+ "WHERE d.client_id = :clientId AND (d.status = 'DA_BAN_HANH') AND (d.id IN ("
			+ "(SELECT d1.id FROM vz.DOCUMENT_OUT d1 INNER JOIN vz.DOCUMENT_RECEIVE r ON d.id = r.doc_id and d.active = TRUE and r.active = true "
			+ "LEFT JOIN vz.SYS_USER u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receive_id = u.org_id AND u.id = :userId "
			+ "WHERE d.status = 'DA_BAN_HANH' AND (r.status IS NULL OR r.status = 'NOT_YET') AND "
			+ "((r.type ='FORWARD' AND r.receive_id = :userId) OR (r.type ='USER' AND r.receive_id = :userId) OR (r.type ='ORG' AND u.lead is TRUE AND r.receive_id = u.org_id ) OR (r.type ='ALL' AND r.receive_id = u.org_id ))))"
			+ " OR (d.list_signers_name IS NOT NULL AND u3.full_name IN (SELECT TRIM(UNNEST(string_to_array(d.list_signers_name, ','))))) "
			+ "	OR (d.list_signers_id IS NOT NULL AND u3.id IN (SELECT CAST(TRIM(UNNEST(string_to_array(d.list_signers_id, ','))) AS INTEGER))) "
			+ ")"
			+ " AND (dr.status IS NULL OR dr.status = 'NOT_YET')"
//			+ "AND (obr IS NULL OR obr.read = FALSE)"
			;
	
	public static final String FOLDER_MENU_QUERY = "SELECT p.folder_id AS fId, p.from_user_id AS fromUserId, p.to_user_id AS toUserId, p.to_org_id AS toOrgId, f.status AS status, '1' AS type "
			+ " FROM vz.HS_FOLDER_PROCESS p "
			+ " INNER JOIN vz.HS_FOLDER f ON f.id = p.folder_id"
			+ " WHERE (p.from_user_id = :userId OR p.to_user_id = :userId OR :orgId is null OR p.to_org_id = :orgId) "
			+ " AND p.client_id = :clientId AND p.active = TRUE AND f.active =TRUE AND f.client_id = :clientId and f.parent_id is null"
			+ UNION_ALL
			+ "SELECT f.id, '0', '0', '0', '0', '2' FROM vz.HS_FOLDER f "
			+ " INNER JOIN vz.HS_FOLDER_SHARE s ON f.id = s.folder_id "
			+ " INNER JOIN vz.SYS_CATEGORY c ON c.id = f.maintenance "
			+ " WHERE s.client_id = :clientId AND s.active = TRUE AND f.active =TRUE AND f.client_id = :clientId "
			+ " AND ((f.folder_type = 'COQUAN' AND f.org_ql_id = :orgId) OR (f.folder_type = 'CANHAN' AND s.user_id = :userId)) and f.parent_id is null"
//			+ " AND f.status = 'HS_TAI_LIEU' AND f.parent_id IS NULL"
//			+ " AND :type = CASE WHEN c.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN c.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END "
			;
	
	public static final String SIGNER_DOCUMENT_OUT_QUERY = 
			"SELECT d.id as id FROM vz.DOCUMENT_OUT d "
			+ " INNER JOIN vz.SYS_USER u3 ON u3.id = :userId "
			+ " WHERE d.active = TRUE  AND d.client_id = :clientId AND d.status = 'DA_BAN_HANH' AND ( "
			+ "(d.list_signers_name IS NOT NULL AND u3.full_name IN (SELECT TRIM(UNNEST(string_to_array(d.list_signers_name, ','))))) "
			+ "OR (d.list_signers_id IS NOT NULL AND u3.id IN (SELECT CAST(TRIM(UNNEST(string_to_array(d.list_signers_id, ','))) AS INTEGER))) "
			+ ")"
		;

	public static final String PID_QUERY = 
			" SELECT p.id FROM vz.SYS_DOCUMENT_IN_PROCESS p "
			+ " INNER JOIN (SELECT p1.doc_id , MAX(p1.step) as step FROM vz.SYS_DOCUMENT_IN_PROCESS p1 WHERE p1.active = TRUE AND p1.client_id =:clientId "
			+ " AND p1.to_user in(:userIds) OR p1.delegater_id in(:userIds) GROUP BY p1.doc_id ) gd ON gd.doc_id = p.doc_id AND gd.step = p.step";
	
	public static final String SIGNER_NAME_DOCUMENT_OUT_QUERY = 
			"SELECT u.id AS id, u.full_name AS fullName, o.name AS orgName, c.name AS position, u.phone AS phone FROM vz.SYS_USER u"
			+ " INNER JOIN vz.SYS_ORGANIZATION o ON o.id = u.org_id"
			+ " INNER JOIN vz.SYS_CATEGORY c ON c.id = u.position_id "
			+ " INNER JOIN vz.DOCUMENT_OUT d ON d.create_by = :userId "
			+ " WHERE u.active = TRUE AND u.full_name IN ("
			+ " SELECT TRIM(UNNEST(string_to_array(d.list_signers_name, ',')))) "
			+ " AND (:text IS NULL OR LOWER(u.full_name) LIKE CONCAT('%', CAST(:text AS TEXT), '%'))"
			+ " AND d.client_id = :clientId AND d.create_by = :userId AND d.list_signers_name IS NOT NULL "
			+ " ORDER BY d.create_date DESC"
		;
}
