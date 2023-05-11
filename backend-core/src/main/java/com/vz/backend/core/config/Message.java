package com.vz.backend.core.config;

public class Message {
	public static final String EXIST_TYPE_CATEGORY = "Loại danh mục mới đã tồn tại ";
	public static final String EXIST_CATEGORY = "Danh mục mới đã tồn tại ";
	public static final String EXIST_USER_NAME = "Tên người dùng mới đã tồn tại ";
	public static final String INVALID_UPDATE_TASK = "Tài khoản hiện tại không có quyền truy cập vào công việc hiện tại";
	public static final String REJECT_UPDATE_TASK = "Bạn bị từ chối gia hạn công việc";
	public static final String NO_CREATE_DOC = "Lỗi tài khoản hiện tại không được phép tiếp nhận/ từ chối văn bản";
	public static final String NOT_FOUND_CLIENT = "Lỗi không tìm thấy bộ phận phù hợp";
	public static final String NOT_FOUND_POSITION = "Lỗi không tìm thấy danh mục chức danh";
	public static final String NOT_FOUND_ORG = "Lỗi không tìm thấy tổ chức phù hợp";
	public static final String ORG_PARENTID_ERROR = "Đơn vị cha không hợp lệ";
	public static final String NOT_FOUND_USER = "Lỗi không tìm thấy tài khoản phù hợp";
	public static final String NOT_FOUND_CATEGORY = "Lỗi không tìm thấy danh mục phù hợp";
	public static final String NOT_FOUND_CATEGORY_LDAP = "Danh mục này không được phép sửa tên";
	public static final String INVALID_FIELD = "Thuộc tính động không hợp lệ ";
	public static final String MAX_LENGTH_FIELD = "Tên thuộc tính chỉ chứa tối đa 20 kí tự";
	public static final String NOT_FOUND_OBJECT = "Lỗi không tìm thấy đối tượng phù hợp";
	public static final String NOT_FOUND_FIELD = "Lỗi không tìm thấy thuộc tính phù hợp ";
	public static final String TYPE_DATA_FIELD = "Kiểu dữ liệu thuộc tính động kiểu dữ liệu không hợp lệ ";
	public static final String INVALID_UPDATE_TYPE_DATA = "Kiểu dữ liệu thuộc tính động không được thay đổi";
	public static final String EXIST_FIELD = "Thuộc tính mới đã tồn tại ";
	public static final String INVALID_ACCOUNT = "Tài khoản không hợp lệ ";
	public static final String INVALID_PAGE = "Số trang không hợp lệ ";
	public static final String INVALID_NUMBER_ARRIVAL = "Số đến đã tồn tại";
	public static final String INVALID_NUMBER_SIGN = "Số văn bản đến đã tồn tại";
	public static final String ERROR_SYS = "Lỗi hệ thống, vui lòng thử lại sau";
	public static final String INVALID_NUMBER_FIELD = "Gía trị thuộc tính động phải là kiểu số";
	public static final String INVALID_DATE_FIELD = "Gía trị thuộc tính động phải là kiểu ngày format YYYY-MM-DD";
	public static final String INVALID_OPTION_FIELD = "Trường tùy chọn của thuộc tính động không hợp lệ ";
	public static final String NO_DATA_FIELD = "Dữ liệu đang trống ";
	public static final String NO_INPUT_DATA = "Không có dữ liệu đầu vào ";
	public static final String NOT_FOUND_OBJECT_DATA = "Lỗi không tìm thấy dữ liệu của đối tượng phù hợp";
	public static final String INVALID_ID = "Chỉ số định danh không hợp lệ ";
	public static final String NOT_FILE_FOUND = "Lỗi không tìm thấy tệp cũ để xóa";
	public static final String REQUIRE_FIELD = "Lỗi trường bắt buộc nhập chưa có dữ liệu";
	public static final String INVALID_LABEL = "Lỗi nhãn dán của trường tùy chọn bị trùng lặp ";
	public static final String NOT_FOUND_DOC = "Lỗi không tìm thấy văn bản phù hợp ";
	public static final String NO_TRANSFER_HANDLE = "Lỗi tài khoản đăng nhập không được phép truy cập vào văn bản hiện tại";
	public static final String ROLE_LIBRARIAN = "Chức năng này chỉ dành cho văn thư";
	public static final String DOCUMENT_NOT_FOUND = "Không tìm thấy văn bản";
	public static final String WRONG_INPUT_DATA = "Dữ liệu đầu vào thiếu hoặc không đúng";
	public static final String WRONG_CONDITION_DATE = "Thời gian bắt đầu phải bé hơn thời gian kết thúc";
	public static final String WRONG_INPUT_YEAR = "Năm không đúng định dạng";
	public static final String ACTION_FAILED = "Thao tác không thành công";
	public static final String NOT_FOUND_FILE = "Lỗi không tìm thấy tệp phù hợp";
	public static final String NO_RETURN_DOC = "Lỗi tài khoản hiện tại không được phép trả lại văn bản";
	public static final String INVALID_PROCESS = "Trạng thái xử lý không cho phép thực hiện thao tác này";
	public static final String NO_PROCESS_HANDLE = "Lỗi tài khoản đăng nhập không được phép truy cập vào văn bản hiện tại";
	public static final String NO_TRANSFER_HANDLE_PROCESS = "Lỗi tài khoản đăng nhập không được phép chuyển xử lý cho văn bản hiện tại";
	public static final String NO_RETAKE = "Lỗi tài khoản đăng nhập không được phép thu hồi hoặc văn bản hiện tại đã hoàn thành";
	public static final String NO_DONE_PROCESS = "Lỗi tài khoản đăng nhập đã hoàn thành xử lý văn bản hiện tại";
	public static final String NO_PROCESS_WHEN_DONE_DOC = "Văn bản này không được phép thu hồi hoặc trả lại";
	public static final String ATTACHMENT_FILE_NOT_PERMIT = "Tài khoản đăng nhập không được phép tải văn bản này";
	public static final String NO_PERMISSION = "Không có quyền truy cập";
	public static final String NO_ACTION_PERMISSION = "Không có quyền thao tác";
	public static final String NO_ACCESS_PERMISSION = "Bạn không có quyền xem chi tiết văn bản này";
	public static final String NOTIFICATION_NOT_FOUND = "Thông báo không tồn tại hoặc đã bị xóa";
	public static final String ROLE_NOT_FOUND = "Không tìm thấy vai trò phù hợp";
	public static final String ATTACHMENT_FILE_NOT_FOUND = "Không có file đính kèm";
	public static final String CMT_NOT_FOUND = "Không tìm thấy bình luận phù hợp";
	public static final String NO_DELETE_DOC = "Lỗi tài khoản đăng nhập không được phép xóa văn bản hiện tại";
	public static final String DELETE_NOT_ALLOWED = "Người tạo văn bản mới được phép xóa";
	public static final String ISSUED_NOT_ALLOWED = "Không được phép ban hành văn bản này";
	public static final String UPDATE_NOT_ALLOWED = "Không được phép cập nhật văn bản";
	public static final String RETAKE_NOT_ALLOWED = "Văn bản chưa trình ký hoặc đã hoàn thành không thể thu hồi";
	public static final String PERSON_NOT_ALLOWED = "Người tạo văn bản mới được phép thực hiện thao tác này";
	public static final String ORG_NAME_INVALID = "Tên tổ chức không hợp lệ";
	public static final String ORG_NOT_SAME = "Người dùng này không cùng tổ chức với bạn";
	public static final String ORG_NAME_EXIST = "Tên tổ chức đã tồn tại";
	public static final String ORG_PHONE_EXIST = "Số điện thoại đã tồn tại";
	public static final String ORG_ID_CODE_EXIST = "Mã định danh của tổ chức đã tồn tại";
	public static final String ORG_IDENTIFIER_EXIST = "Mã cơ quan của tổ chức đã tồn tại";
	public static final String ORG_ID_CODE_INVALID = "Mã định danh của tổ chức không được để trống";
	public static final String FIELD_INVALID = " không hợp lệ";
	public static final String NO_INPUT = " không được bỏ trống.";
	public static final String NO_PROCESS_DOC = "Lỗi tài khoản đăng nhập không có quyền truy cập văn bản hiện tại";
	public static final String NO_DELEGATE = "Ủy quyền hết hiệu lực";
	public static final String NO_HS = "Bạn cần tạo hồ sơ công việc";
	public static final String CLIENT_EXPIRE = "Khách hàng đã hết hạn dùng thử.";
	public static final String CLIENT_EXPIRED = "Khách hàng đã hết hạn dùng thử.";
	public static final String FAIL_LOGIN = "Tài khoản đăng nhập không hợp lệ";
	public static final String CLIENT_CODE_UNIQUE = "Mã khách hàng không được trùng lặp";
	public static final String DB_BOOK_YEAR = "Đề nghị nhập năm sổ văn bản";
	public static final String VBUQ_NOT_FOUND = "Không tìm thấy văn bản ủy quyền hợp lệ";
	public static final String CAN_NOT_ACTION = "Trạng thái văn bản không cho phép thực hiện thao tác này";
	public static final String PASSWORD_INCORRECT = "Mật khẩu cũ không đúng";
	public static final String UPDATE_PROCESS = "Tiến độ từ 0 - 100";
	public static final String TASK_NOT_FOUND = "Không tìm thấy công việc phù hợp";
	public static final String DOCUMENT_BOOK_UPDATE = "Đơn vị quản lý mới có quyền chỉnh sửa sổ văn bản";
	public static final String CALENDER_NOT_ALLOW = "Lỗi tài khoản đăng nhập không được phép truy cập vào danh mục quản lý lịch";
	public static final String CALENDER_FAIL_REGISTER_BAN = "Đăng kí lịch lên Ban không thành công";
	public static final String CALENDAR_INVALID_TIME = "Thời gian đăng kí không hợp lệ";
	public static final String CALENDAR_INVALID_DESCRIPTION = "Lịch chưa có nội dung";
	public static final String CALENDAR_INVALD = "Không tìm thấy lịch phù hợp";
	public static final String CALENDAR_NOT_UPDATE = "Chỉ có người tạo mới sửa được lịch";
	public static final String CALENDAR_JOIN_INVALID = "Đề nghị nhập thông tin cá nhân tham gia lịch làm việc";
	public static final String CALENDAR_DATA_INVALD = "Dữ liệu truyền vào không hợp lệ";
	public static final String TASK_CMT_NOT_FOUND = "Không tìm thấy nội dung phù hợp";
	public static final String DOCUMENT_BOOK_NOT_FOUND = "Không tìm thấy sổ văn bản phù hợp";
	public static final String DOCUMENT_STATUS_NOT_ALLOW = "Trạng thái văn bản không cho phép thực hiện thao tác này";
	public static final String SET_DEFAULT_NOT_YET = "Lỗi chưa thiết lập tài khoản mặc định cho phòng ban đích";
	public static final String REVIEW_REQUIRED = "Văn bản chưa được đánh giá không thể chuyển xử lý hoặc hoàn thành";
	public static final String DUPLICATE_TOKEN = "Chứng thư số này đã được sử dụng";
	public static final String ERROR_TOKEN = "Lỗi chứng thực số không hợp lệ";
	public static final String NUMBER_IN_BOOK_EXIST = "Số văn bản đã tồn tại";
	public static final String FOLDER_NOT_FOUND = "Không tìm thấy thư mục";
	public static final String REQUIRES_RETURN_DOC_REASON = "Bạn cần nhập lý do trả lại văn bản";
	public static final String NO_DEL_CALENDAR = "Bạn không có quyền lịch này";
	public static final String MAIL_SYSTEM_ERROR = "Lỗi hệ thống từ dịch vụ thư điện tử, vui lòng thử lại sau";
	public static final String ROOM_NAME_INVALID= "Bạn phải nhập tên phòng họp";
	public static final String QUANTITY_ROOM_INVALID = "Số lượng người không hợp lệ";
	public static final String ROOM_INVALID = "Lịch họp chưa có phòng họp";
	public static final String ADDRESS_OUTSIDE_INVALID = "Lịch họp chưa có địa điểm họp ngoại viện";
	public static final String ROOM_TIME_DUPLICATE = "Đã tồn tại lịch họp ở khung thời gian này";     
	public static final String TASK_EXE_INVALID = "Chưa chọn người thực hiện công việc";
	public static final String TASK_APPROVER_INVALID = "Chưa chọn người duyệt công việc";
	public static final String ROOM_NOT_FOUND = "Không tìm thấy phòng họp phù hợp";
	public static final String ROOM_USING = "Phòng họp đang được sử dụng";
	public static final String NOT_CREATE_SCHEDULE = "Bạn không phải người tạo nhắc việc";
	public static final String NOT_EXIST_SCHEDULE = "Không tồn tại Nhắc việc với id: ";
	public static final String NO_FINISH_TASK = "Chưa đủ điều kiện để hoàn thành văn bản";
	public static final String TEMPLATE_FILE_INVALID = "Văn bản mẫu phải là tệp word";
	public static final String TEMPLATE_NOT_FOUND = "Không tìm thấy mẫu văn bản phù hợp";
	public static final String NOT_FOUND_FILES = "Lỗi không tìm thấy tệp phù hợp";
	public static final String NOT_FOUND_FORMULA = "Lỗi không tìm thấy đối tượng phù hợp";
	public static final String WEIGHT_INVALID = "Phân bổ trọng số từ 0-100";
	public static final String KPI_SET_DATA_INVALID = "Dữ liệu truyền vào không hợp lệ (có thể tên đã tồn tại, dữ liệu không khớp..)";
	public static final String KPI_SET_NOT_FOUND = "Bộ chỉ tiêu không tìm thấy";
	public static final String KPI_NOT_FOUND = "Chỉ tiêu không tìm thấy";
	public static final String KPI_APPLICATION_NO_USER = "Bộ KPI phải giao cho cá nhân hoặc tổ chức";
	public static final String KPI_APPLICATION_DATE_START = "Ngày đầu kì đánh giá không hợp lệ";
	public static final String KPI_SET_USER_UNIQUE = "Mỗi người chỉ được giao một bộ KPI";
	public static final String NOT_FOUND_KPI_APP = "Lỗi không tìm thấy phiếu giao KPI phù hợp";
	public static final String NOT_FOUND_KPI_TARGET = "Lỗi không tìm thấy chỉ tiêu phù hợp";
	public static final String PARENT_TASK_INVALID = "Công việc cha không hợp lệ";
	public static final String INVALID_TARGETS = "Mục tiêu phải lớn hơn 0";
	public static final String KPI_CAL_SYS_ERROR = "Lỗi hệ thống khi tính toán KPI";
	public static final String NOT_FOUND_KPI_CONDITION = "Không tìm thấy KPI phù hợp với điều kiện tìm kiếm";
	public static final String FORMULA_USING = "Công thức đang được sử dụng";
	public static final String KPI_SET_USING = "Bộ chỉ tiêu đang được sử dụng";
	public static final String INVALID_FILE_CODE = "Mã hồ sơ phải định dạng 000.00.00.X00.0000.00.XX";
	public static final String INVALID_TITLE_FOLDER = "Tên hồ sơ hoặc mã hồ sơ đã tồn tại";
	public static final String INVALID_HEADINGS = "Đề mục cha không hợp lệ";
	public static final String INVALID_ROLE_MANAGE_HEADINGS = "Bạn chưa có quyền thêm đề mục";
	public static final String INVALID_DELETE_HEADINGS = "Thao tác không hợp lệ, đề mục này đang được sử dụng";
	public static final String TYPE_NOT_EXIST = "Không tồn tại type này";
	public static final String NOT_FOUND_FONT = "Lỗi không tìm thấy phông phù hợp ";
	public static final String INVALID_FONT_NAME = "Tên phông không hợp lệ ";
	public static final String INVALID_FONT_ORGANLD = "Mã phông không hợp lệ ";
	public static final String FONT_IN_USE = "Phông đang được sử dụng";
	public static final String INVALID_FONT_DATA = "Dữ liệu đang bị trùng lặp hoặc lỗi hệ thống, vui lòng thử lại sau";
	public static final String NOT_FOUND_ENCRYPTION = "Không tìm thấy đối tượng mã hóa";
	public static final String ENCRYPTION_ERROR = "Lỗi trong quá trình mã hóa";
	public static final String FILE_DOCUMENT_EXISTS = "File văn bản đang tồn tại trong folder đích";
	public static final String ENCRYPTED_FILE_KEY_INVALID= "Lỗi dữ liệu khóa của tệp mã hóa không hợp lệ";
	public static final String ENCRYPTED_FILE_INVALID= "Lỗi dữ liệu của tệp mã hóa không hợp lệ";
	public static final String SECRETARY_ERROR = "Lỗi phân công thư kí";
	public static final String DELETE_DONE_TASK_NOT_ALLOWED = "Công việc đã hoàn thành không thể xóa";
	public static final String CALENDAR_INVALID_PARTICIPANTS = "Lỗi chưa nhập thành phần tham gia";
	public static final String WRONG_INPUT_TIME_DATA = "Dữ liệu thời gian đầu vào thiếu hoặc không đúng";
	public static final String NO_TRANSFER_TASK = "Lỗi tài khoản đăng nhập không thể chuyển tiếp công việc hiện tại";
	public static final String NO_MAIN_TRANSFER_TASK = "Bạn chưa chọn xử lý chính cho bước xử lý này";
	public static final String ONE_MAIN_TRANSFER_TASK = "Thao tác không hợp lệ, xử lý chính chỉ được phép chọn 1 ";
	public static final String ORG_NO_LEAD_TRANSFER_TASK = "Tổ chức thực hiện chưa có trưởng phòng";
	public static final String TASK_STATUS_NO_ACTION = "Trạng thái công việc không cho phép bạn thực hiện thao tác này";
	public static final String TASK_EXE_STATUS_NO_ACTION = "Trạng thái thực hiện công việc không cho phép bạn thực hiện thao tác này";
	public static final String NOT_FOUND_DOC_INTERNAL_APPROVE = "Bạn không có quyền để duyệt hoặc góp ý cho văn bản này";
	public static final String NOT_FOUND_DOCUMENT_INTERNAL = "Không tìm thấy văn bản nội bộ";
	public static final String DOC_INTERNAL_INVALID_STATUS = "Trạng thái không cho phép thực hiện hành động này";
	public static final String DOC_INTERNAL_INVALID_USER = "Bạn không có quyền thực hiện hành động này";
	public static final String NOT_FOUND_DOC_INTERNAL_COMMENT = "Không tìm thấy góp ý cho văn bản nội bộ";
	public static final String NOT_FOUND_TAG = "Không tìm thấy thẻ phù hợp";
	public static final String INVALID_OBJECT_TAG_TYPE = "Đối tượng gán thẻ không phù hợp";
	public static final String NOT_FOUND_DOC_IN = "Không tìm thấy văn bản đến phù hợp";
	public static final String NOT_FOUND_DOC_OUT = "Không tìm thấy văn bản đi phù hợp";
	public static final String OBJECT_TAG_EXIST = "Tài liệu hoặc công việc đã được gán thẻ này";
	public static final String OBJECT_TAG_NOT_FOUND = "Không tìm thấy tài liệu hoặc công việc được gán thẻ này";
	public static final String TAG_EXIST = "Thẻ này đã tồn tại";
	public static final String DOC_INTERNAL_INVALID_REGISTRATION = "Đăng ký bắt buộc phải chọn người ký";
	public static final String EMAIL_INVALID = "Địa chỉ thư điện tử không hợp lệ";
	public static final String DUPLICATE_EMAIL = "Thư điện tử này đã có người sử dụng";
	public static final String EXIST_ROLE_NAME = "Tên vai trò mới đã tồn tại ";
	public static final String INVALID_INPUT_DATA = "Dữ liệu đầu vào không hợp lệ";
	public static final String MEETING_CALENDAR_NOT_FOUND = "Không tìm thấy lịch họp phù hợp";
	public static final String MEETING_CALENDAR_ATTACHMENT_NOT_FOUND = "Không tìm thấy tệp đính kèm lịch họp phù hợp";
	public static final String MEETING_CALENDAR_INVITATION_DELETE_WARNING = "Bạn không thể xóa giấy mời của phiên họp";
	public static final String NOT_FOUND_CONNECT_SYSTEM = "Lỗi không tìm thấy hệ thống kết nối phù hợp";
	public static final String EXIST_CONNECT_SYSTEM = "Hệ thống kết nối đã tồn tại";
	public static final String INVALID_DATA_CONNECT_SYSTEM = "Thông tin tổ chức đăng kí liên kết ngoài của hệ thống đích không khớp với dữ liệu truyền vào";
	public static final String NO_INPUT_CONNECT_SYSTEM = "Lỗi không truyền vào dữ liệu hệ thống kết nối";
	public static final String NO_SETTING_ORG_CONNECT_SYSTEM = "Hệ thống kết nối chưa cấu hình tổ chức liên kết";
	public static final String NO_SETTING_USER_CERT = "Hệ thống kết nối chưa cấu hình chứng thực số cho các cá nhân";
	public static final String ERROR_ADD_ENCRYPT = "Lỗi dữ liệu yêu cầu chia sẻ tệp mã hóa chưa đúng";
	public static final String ERROR_OUTSIDE_OBJECT = "Đây không phải là đối tượng liên kết với hệ thống ngoài";
	public static final String EMPTY_CLERICAL_HANDLE = "Không có văn thư xử lý";

	public static final String REPORT_NOT_FOUND = "Báo cáo không tồn tại";
	public static final String REPORT_TYPE_REPORT = "Loại báo cáo không hợp lệ";
	public static final String REPORT_TYPE = "Loại không hợp lệ";
	public static final String REPORT_SIGNER_NOT_FOUND = "Người ký không tồn tại";
	public static final String REPORT_POSITION_TITLE_NOT_FOUND = "Nhan đề không tồn tại";
	public static final String REPORT_TITLE_NOT_FOUND = "Bạn chưa nhập tiêu đề báo cáo";
	public static final String REPORT_DUYET_NOT_UPDATE = "Báo cáo đã duyệt ! Không được phép chỉnh sửa";
	public static final String ACTION_STATUS = "Thao tác duyệt không hợp lê !";
	public static final String ERROR_NUMBER_IN_BOOK = "Số văn bản không hợp lệ!";
	public static final String CONFIRM_NUMBER = "Số xác nhận đã tồn tại !";

	public static final String BPMN_IS_USED = "Luồng đã được sử dụng. Không thể xóa !";
	public static final String CLIENT_IAM_NOT_FOUND = "Client IAM không đúng !";
	public static final String USER_LINK_IAM = "Tài khoản IAM đã được liên kết với tài khoản idoc khác !";
	public static final String USER_NAME_IAM_MODIFIED = "Không được phép sửa tên đăng nhập !";
}
