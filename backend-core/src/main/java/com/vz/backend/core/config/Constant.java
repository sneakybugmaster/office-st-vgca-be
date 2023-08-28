package com.vz.backend.core.config;

public class Constant {
	public static final int DEFAULT_LENGTH_PASSWORD_FILE = 8;
	public static final int NUMBER_OF_PAGE = 10;
	public static final int MAX_LENGTH_NAME = 20;
	public static final String UPDATE = "update";
	public static final String SAVE = "save";
	public static final String THVV = "THVV";
	public static final String THHS = "THHS";
	public static final String CAT_ORG_OTHER = "DVN";
	public static final String CAT_ORG_TYPE = "LDV";
	public static final String CAT_POSITION = "CVND";
	public static final String CAT_DOC_STATUS = "TTVB";
	public static final String CAT_OBJECT = "LDT";
	public static final String CAT_ORG_ISSUED = "TCBH";
	public static final String CAT_CUSTOMER_TYPE = "LKH";
	public static final String CAT_DOC_TYPE = "LVB";
	public static final String CAT_SECURE = "DMVB";
	public static final String CAT_URENT = "DKVB";
	public static final String CAT_FIELD = "LVVB";
	public static final String CAT_TYPE_RECEIVE = "PTNVB";
	public static final String USER_MANUAL_CODE = "USER_MANUAL";
	public static final String USER_MANUAL_NAME = "Hướng dẫn sử dụng";
	public static final String NAME_POSITION = "Chức vụ";
	public static final String RETURN_DOC = "Trả lại văn bản";
	public static final String RETAKE_DOC = "Thu hồi văn bản";
	public static final String DOING = "Đang xử lý";
	public static final String NOT_YET = "Chờ xử lý";
	public static final String DONE = "Hoàn thành";
	public static final Long BOOK_DOC_VB_DI = 1L;
	public static final Long BOOK_DOC_VB_DEN = 0L;
	public static final Long BOOK_DOC_VB_DI_NOI_BO = 2L;
	public static final int RE_OPEN_TASK_TYPE = 3;
	public static final int TRANSFER_HANDLE_TYPE = 1;
	public static final int RETURN_DOC_TYPE = 2;
	public static final int OTHER_ACTION_TYPE = 0;
	public static final int ORG_TRANSFER_TYPE = 4;

	public static final String MAIN = "Xử lý chính";
	public static final String SUPPORT = "Xử lý phối hợp";
	public static final String SHOW = "Nhận để biết";
	public static final String DIRECTION = "Chỉ đạo";
	public static final String PASSWORD_DEFAULT = "123456";
	public static final String ACTION_ADD = "Thêm mới";
	public static final String ACTION_COMMING_TO = "Mới đến";
	public static final String ACTION_TRANFER_HANDLE = "Chuyển xử lý";
	public static final String ACTION_RETURN_DOC = "Trả lại văn bản";
	public static final String ACTION_COMMENT = "Nhận xét";
	public static final String ACTION_ISSUED = "Ban hành";
	public static final String ACTION_READ = "Đã xem";
	public static final String ACTION_LOGIN = "Đăng nhập";
	public static final String ACTION_DONE = "Đã xử lý";
	public static final String ACTION_UNKNOW = "Chưa xác định";

	public static final int CALENDAR_WAIT = 0;
	public static final int CALENDAR_APPROVE = 1;
	public static final int CALENDAR_REJECT = 2;

	public static final String RECEIVER_TYPE_ORG = "ORG";
	public static final String RECEIVER_TYPE_USER = "USER";
	public static final String RECEIVER_TYPE_ALL = "ALL";
	public static final String RECEIVER_TYPE_FORWARD = "FORWARD";
	public static final String LINK_SIGN = "/";
	public static final int DESCRIPTION_LENGTH = 20;
	public static final int ORG_NAME_LENGTH = 100;
	public static final int ORG_PHONE_LENGTH = 50;
	public static final int ORG_ADDRESS_LENGTH = 200;
	public static final int ORG_EMAIL_LENGTH = 50;
	public static final int COMMENT_LENGTH = 2000;
	public static final String ORG_PHONE_NAME = "Số điện thoại";
	public static final String ORG_ADDRESS_NAME = "Địa chỉ";
	public static final String ORG_NAME = "Tên đơn vị";
	public static final String ORG_EMAIL_NAME = "Thư điện tử";
	public static final String ORG_TYPE_NAME = "Loại đơn vị";
	public static final String ORG_ORDER_NUMBER = "Số thứ tự";

	// default sort by
	public static final String DEFAULT_SORT_BY = "${configs.sort-by: CREATEDATE}"; //UPDATEDATE
	public static final String DEFAULT_DIRECTION = "DESC";
	public static final String DEFAULT_PAGE_SIZE = "10";
	public static final int DEFAULT_PAGE_SIZE_INT = 10;
	public static final String DEFAULT_PAGE_NUMBER = "1";

	// BCY cấp đơn vị
	public static final String BAN = "Ban";
	public static final String CUC_VU_VIEN = "Cục Vụ Viện";
	public static final String PHONG = "Phòng";

	// for Task
	public static final Long TYPE_TASK = 1L;
	public static final Long TYPE_TASK_CMT = 2L;
	public static final Long TYPE_WORD_EDITOR = 3L;
	public static final Long TYPE_WORD_EDITOR_CMT = 4L;
	public static final Long TYPE_FOLDER_FORM = 5L;

	public static final String TYPE_ROOM = "room"; // phòng họp
	public static final String TYPE_CATEGROY = "category"; // loại họp : họp giao ban, khẩn cấp,..

	// statusType, //1- register /2-approve /3-publish
	// orgType, //1-Ban /2-CucVuVien /3-Phong
	public static final int CALENDAR_SCREEN_REGISTER = 1;
	public static final int CALENDAR_SCREEN_APPROVE = 2;
	public static final int CALENDAR_SCREEN_PUBLISH = 3;
	public static final int CALENDAR_ORG_TYPE_BAN = 1;
	public static final int CALENDAR_ORG_TYPE_CUC_VU_VIEN = 2;
	public static final int CALENDAR_ORG_TYPE_PHONG = 3;

	public static final float CM_TO_POINT_CONVERSION_RATE = 28.35f;

	public static final String ORG_NAME_UPPER = "VỤ KHOA HỌC CÔNG NGHỆ";

	public static final String INCOMING_STRING_LITERAL = "ĐẾN";

	// hết hạn client
	public static final String TOKEN_H03 = "Cục H03";
	public static final String TOKEN_H05 = "Bộ công an H05";
	public static final String TOKEN_BCY = "Ban cơ yếu BCY";

	public static final String DEFAULT_EXPIRED_START_DATE_H05 = "14/11/2020";
	public static final String DEFAULT_EXPIRED_START_DATE_H03 = "08/08/2024";
	public static final String DEFAULT_EXPIRED_START_DATE_BCY = "01/12/2020";
	
	//start node
	public static final Long START_NODE = 0L;
	public static final Integer START_STEP = 1;
	public static final Integer NO_STEP = 0;
	
	// for xin ý kiến
	public static final String TYPE_XIN_Y_KIEN = "XIN_Y_KIEN";
	public static final String TYPE_CHO_Y_KIEN = "CHO_Y_KIEN";
	
	// for File
	public static final int TYPE_ATT = 1;
	public static final int TYPE_ATT_CMT = 2;
	
	//file tag
	public static final String JPG = ".JPG";
	public static final String JPGS = ".jpg";
	public static final String PNG = ".PNG";
	public static final String PNGS = ".png";
	public static final String DOC = ".DOC";
	public static final String DOCS = ".doc";
	public static final String DOCX = ".DOCX";
	public static final String DOCXS = ".docx";
	public static final String ODT = ".ODT";
	public static final String ODTS = ".odt";
	public static final String PDF = ".PDF";
	public static final String PDFS = ".pdf";

	public static final String WEEK = "week";
	public static final String YEAR = "year";
	public static final String MONTH = "month";
	public static final String DAY = "day";
	
	// file path
	public static final String ENCRYPT_FILE_PATH = "encrypt_file";
	
	// ecabinet
	public static final String FILE = "file";
	public static final String TEXT = "text";
	
	// Chuyên viên role
	public static final String CHUYEN_VIEN = "Chuyên viên";
	public static final String VU_TRUONG = "Vũ trưởng";
	
	// document type
	public static final String DOC_IN_TRANSFER = "doc_in_transfer";
	public static final String DOC_IN_RETURN = "doc_in_return";
	public static final String DOC_IN_DONE = "doc_in_done";
	public static final String DOC_IN_ORG_TRANSFER = "doc_in_org_transfer";
	public static final String DOC_OUT_INTERNAL = "doc_out_internal";
	public static final String DOC_IN_ADD = "doc_in_add";
	public static final String DOC_IN_COMMENT = "doc_in_comment";
	public static final String DOC_OUT_ADD = "doc_out_add";
	public static final String DOC_OUT_DONE = "doc_out_done";
	public static final String DOC_OUT_COMMENT = "doc_out_comment";
	public static final String TASK = "task";
	public static final String TASK_COMMENT = "task_comment";
	public static final String DOC_INTERNAL = "doc_internal";
	public static final String DOC_INTERNAL_COMMENT = "doc_internal_comment";
	public static final String CALENDAR = "calendar";
	
	// Trạng thái hồ sơ nộp lưu
	public static final Integer HS_NEW = 1;
	public static final Integer HS_REJECT = 4;
	public static final Integer HS_APPROVE = 5;
	public static final Integer HS_PLUS = 2;
	
	// Vai trò Ecabinet
	public static final String THU_KI = "Thư kí";
	
	// object task by type
	public static final int USER = 0;
	public static final int GROUP = 1;
	public static final int ORG = 2;
	
	// Module buff when assign role from cabinet to office
	public static final String CABINET = "CABINET";
	public static final String CABINET_ADMIN_USERS = "CABINET_ADMIN_USERS";
	public static final String CABINET_ADMIN_THREADS = "CABINET_ADMIN_THREADS";
	public static final String CABINET_ADMIN_ROLES = "CABINET_ADMIN_ROLES";
	public static final String CABINET_ADMIN_ROOMS = "CABINET_ADMIN_ROOMS";
	public static final String CABINET_ADMIN = "CABINET_ADMIN";
	public static final String CABINET_GROUP = "CABINET_GROUP";
	public static final String OFFICE_ORG = "ORG";
	public static final String OFFICE_CATEGORY = "CATEGORY";
	public static final String OFFICE_ADMIN = "ADMIN";
	public static final String OFFICE_CALENDAR = "CALENDAR";
	
	// Phòng họp không giấy tờ Office
	public static final String CAL_CABINET_MEETING = "CAL_CABINET_MEETING";
	
	//Vai trò, module Quản trị vận hành
	public static final String OFFICE_ROLE_ADMIN = "Quản trị vận hành";
	public static final String CABINET_ROLE_ADMIN = "Cabinet - Quản trị vận hành";
	public static final String OFFICE_ADMIN_ROLE = "ROLE";
	public static final String CAN_BO = "Cán bộ";

	public static final String ORG_BCY = "Ban cơ yếu chính phủ";

	public static final String ORG_CODE_IAM = "000.00.02.I00";
	public static final String ORG_CAT_IAM = "IAM";
	public static final String CLIENT_IAM = "idoc";

	// Chức danh đặc biệt
	public static final String VAN_THU_MAIN = "văn thư văn phòng";
	public static final String POSITION_MAIN = "vụ trưởng";
	public static final String POSITION_SECOND = "phó vụ trưởng";
	public static final String CHANH_VAN_PHONG = "chánh văn phòng";
	public static final String CAN_BO_HANH_CHINH_BV = "cán bộ hành chính bv";

}
