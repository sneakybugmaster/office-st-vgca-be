package com.vz.backend.util;

import java.net.InetAddress;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.Vector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.net.ntp.NTPUDPClient;
import org.apache.commons.net.ntp.TimeInfo;

/**
 * Class to provide methods process date
 *
 */

public class DateTimeUtils {

	public static final String TIME_ZONE_ID = "GMT+7:00";
	public static final String TIME_SERVER = "time-a.timefreq.bldrdoc.gov";
	
	//get start date or end date
	public static final int TYPE_START_DATE = 1;
	public static final int TYPE_END_DATE = 2;
	
	public static final String MONDAY = "Thứ Hai";
	public static final String TUESDAY = "Thứ Ba";
	public static final String WEDNESDAY = "Thứ Tư";
	public static final String THURSDAY = "Thứ Năm";
	public static final String FRIDAY = "Thứ Sáu";
	public static final String SATURDAY = "Thứ Bảy";
	public static final String SUNDAY = "Chủ Nhật";
	
	
	private static Log log = LogFactory.getLog(DateTimeUtils.class);

	public static final String YYYY_MM_DD = "yyyy-MM-dd";
	public static final String DD_MM_YYYY = "dd-MM-yyyy";
	// private static String dateFormat = "dd/MM/yyyy";
	public static final String dateStringFormat = "yyyy-MM-dd";
	public static final String DD_MM_YYYY_HH_MM = "dd/MM/yyyy HH:mm";
	public static final String DD_MM_YYYY_HH_MM_SS = "dd/MM/yyyy HH:mm:ss";

	public static final Map<Integer, String> dayOfWeekMap;
	static {
		Map<Integer, String> dateMap = new LinkedHashMap<>();
		dateMap.put(Calendar.MONDAY, MONDAY);
		dateMap.put(Calendar.TUESDAY, TUESDAY);
		dateMap.put(Calendar.WEDNESDAY, WEDNESDAY);
		dateMap.put(Calendar.THURSDAY, THURSDAY);
		dateMap.put(Calendar.FRIDAY, FRIDAY);
		dateMap.put(Calendar.SATURDAY, SATURDAY);
		dateMap.put(Calendar.SUNDAY, SUNDAY);
		dayOfWeekMap = Collections.unmodifiableMap(dateMap);
	}
	
	/**
	 * Lấy khoảng cách ngày giữa 2 ngày
	 *
	 */
	public static int subtractDays(Date date1, Date date2) {
		GregorianCalendar gc1 = new GregorianCalendar();
		gc1.setTime(date1);
		GregorianCalendar gc2 = new GregorianCalendar();
		gc2.setTime(date2);

		int days1 = 0;
		int days2 = 0;
		int maxYear = Math.max(gc1.get(Calendar.YEAR), gc2.get(Calendar.YEAR));

		GregorianCalendar gctmp = (GregorianCalendar) gc1.clone();
		for (int f = gctmp.get(Calendar.YEAR); f < maxYear; f++) {
			days1 += gctmp.getActualMaximum(Calendar.DAY_OF_YEAR);
			gctmp.add(Calendar.YEAR, 1);
		}

		gctmp = (GregorianCalendar) gc2.clone();
		for (int f = gctmp.get(Calendar.YEAR); f < maxYear; f++) {
			days2 += gctmp.getActualMaximum(Calendar.DAY_OF_YEAR);
			gctmp.add(Calendar.YEAR, 1);
		}

		days1 += gc1.get(Calendar.DAY_OF_YEAR) - 1;
		days2 += gc2.get(Calendar.DAY_OF_YEAR) - 1;

		return days1 - days2;
	}

	/**
	 * Đếm số ngày giữa 2 cột mốc thời gian
	 *
	 * @param tungay
	 * @param denngay
	 * @return khoảng cách giữa 2 ngày
	 */
	public static long daysBetween2DateTime(Date tungay, Date denngay) {
		Calendar c1 = Calendar.getInstance(); // new GregorianCalendar();

		Calendar c2 = Calendar.getInstance(); // new GregorianCalendar();

		c1.setTime(tungay);
		c2.setTime(denngay);

		long day = (c2.getTime().getTime() - c1.getTime().getTime()) / (24 * 3600 * 1000);

		return day;
	}

	public static long millisecondsBetween2DateTime(Date tungay, Date denngay) {
		if (tungay == null || denngay == null) {
			return 0;
		}

		return denngay.getTime() - tungay.getTime();
	}

	public static long numberDaysBetween2Dates(Date tungay, Date denngay) {
		if (tungay == null || denngay == null) {
			return 0;
		}
		return daysBetween2Date(tungay, denngay) + 1;
	}

	/**
	 * Trừ 2 ngày lấy số tháng
	 *
	 * @Author ducln
	 * @CreateTime 10:45:01 AM, Jun 2, 2009
	 * @param date1
	 * @param date2
	 * @return
	 */
	public static int getMonthSubtractDays(Date date1, Date date2) {
		int year1 = getYear(date1);
		int year2 = getYear(date2);
		int month1 = getMonth(date1);
		int month2 = getMonth(date2);
		return (year1 - year2) * 12 + month1 - month2;
	}

	/**
	 * Lấy số ngày trong tháng bởi 1 ngày bất kỳ.
	 *
	 * @param date Ngày trong tháng cần xét.
	 * @return Số ngày trong tháng.
	 *
	 */
	public static short daysInMonth(Date date) {
		GregorianCalendar c = new GregorianCalendar();
		c.setTime(date);
		return (short) c.getActualMaximum(Calendar.DAY_OF_MONTH);
	}

	/**
	 * Lấy số ngày của tháng month tại năm year (để không phải khởi tạo lại ngày
	 * truyền vào nữa)
	 *
	 * @param month
	 * @param year
	 * @tag số ngày của tháng, số ngày trong tháng, số lượng ngày trong tháng, số
	 *      lượng ngày của tháng
	 * @return
	 */
	public static int daysOfMonth(int month, int year) {
		Date date = createDate(year, month, 1);
		GregorianCalendar c = new GregorianCalendar();
		c.setTime(date);
		return c.getActualMaximum(Calendar.DAY_OF_MONTH);
	}

	public static long getSystemDate() {
		Calendar calendar = new GregorianCalendar();
		return calendar.getTimeInMillis();
	}

	public static java.sql.Date convertDateUtil2Sql(Date date) {
		return new java.sql.Date(date.getTime());
	}

	public static java.sql.Timestamp convertDateUtil2TimestampSql(Date date) {
		return new java.sql.Timestamp(date.getTime());
	}

	public static String dateToString(Date date) {
		if (date == null) {
			return "";
		}
		return getStringDay(date) + "/" + getStringMonth(date) + "/" + getStringYear(date);
	}

	/**
	 * Chuyển date sang timestamp, theo điều kiện
	 *
	 * @param date
	 * @param isFirstTimeOfDate - Lấy thời gian đầu tiên của ngày (0h0p0s)
	 * @param isLastTimeOfDate  - Lấy thời gian cuối cùng của ngày (23h59p59s)
	 * @return
	 */
	public static java.sql.Timestamp convertDate2TimestampSql(Date date, boolean isFirstTimeOfDate,
			boolean isLastTimeOfDate) {
		String defaultFormat = "dd/MM/yyyy";
		String dateStr = new SimpleDateFormat(defaultFormat).format(date);
		if (isFirstTimeOfDate) {
			dateStr += " 00:00:00";
		}
		if (isLastTimeOfDate) {
			dateStr += " 23:59:59";
		}
		String convertedPattern = "dd/MM/yyyy HH:mm:ss";
		Date _converted;
		try {
			_converted = new SimpleDateFormat(convertedPattern).parse(dateStr);
		} catch (ParseException e) {
			_converted = date;
			log.info(e);
		}
		return convertDateUtil2TimestampSql(_converted);
	}

	/**
	 * Lấy số quý theo tháng
	 *
	 * @param month Tháng cần tìm quý.
	 * @return Quý.
	 *
	 */
	public static Short getQuarterByMonth(Short month) {
		return (short) ((month - 1) / 3 + 1);
	}

	/**
	 * Lấy tháng bắt đầu quý bởi quý.
	 *
	 * @param quarter quý cần tìm tháng bắt đầu.
	 * @return Tháng.
	 *
	 */
	public static int getStartMonthByQuarter(int quarter) {
		return quarter * 3 - 2;
	}

	/**
	 * Convert String to Date java.sql
	 *
	 * @param dateMDY : data value in system format
	 * @return java.sql.Date
	 */
	public static java.sql.Date stringToDateSQL(String dateDMY) {
		if (StringUtils.isNullOrEmpty(dateDMY)) {
			return null;
		}
		int posDay = dateDMY.indexOf("/");
		if (posDay < 0) {
			return null;
		}

		String strDay = dateDMY.substring(0, posDay);
		if (!StringUtils.isDigitString(strDay)) {
			return null;
		}

		int day = Integer.parseInt(strDay);
		if (day < 1 || day > 31) {
			return null;
		}

		int posMonth = dateDMY.indexOf("/", posDay + 1);
		if (posMonth < 0) {
			return null;
		}

		String strMonth = dateDMY.substring(posDay + 1, posMonth);
		if (!StringUtils.isDigitString(strMonth)) {
			return null;
		}
		int month = Integer.parseInt(strMonth);
		if (month < 1 || month > 12) {
			return null;
		}

		String strYear = dateDMY.substring(posMonth + 1);
		if (!StringUtils.isDigitString(strYear)) {
			return null;
		}
		int year = Integer.parseInt(strYear);

		if (month == 2) {
			if (year % 4 == 0) {
				if (day > 29) {
					return null;
				}
			} else if (day > 28) {
				return null;
			}
		}

		if (month == 4 || month == 6 || month == 9 || month == 11) {
			if (day > 30) {
				return null;
			}
		}
		return java.sql.Date
				.valueOf(year + "-" + (month < 10 ? "0" + month : month) + "-" + (day < 10 ? "0" + day : day));
	}

	/**
	 * Convert String to Date Object
	 *
	 * @param dateMDY : data value in system format
	 * @return Date
	 */
	public static Date stringToDate(String dateDMY) {
		if (StringUtils.isNullOrEmpty(dateDMY)) {
			return null;
		}

		String[] array = dateDMY.split("/");
		String result = array[array.length - 1];
		if (result.length() == 2) {
			result = "20" + result;
			int index = dateDMY.lastIndexOf("/");
			String temp = dateDMY.substring(0, index + 1);
			result = temp + result;
		} else {
			result = dateDMY;
		}
		java.sql.Date date = stringToDateSQL(result);
		if (date == null) {
			return null;
		}
		Date temp = new Date(date.getTime());
		return temp;
	}

	public static boolean isDMYDate(String dateDMY) {
		Date date = stringToDateSQL(dateDMY);
		return date != null;
	}

	public static int getCurrentYear() {
		Calendar calendar = new GregorianCalendar();
		return calendar.get(Calendar.YEAR);
	}

	public static int getYear(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int year = cal.get(Calendar.YEAR);
		return year;
	}

	/**
	 *
	 * @param date
	 * @return
	 */
	public static short getDate(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int dateInt = cal.get(Calendar.DATE);
		return ((Integer) dateInt).shortValue();
	}

	public static String getStringYear(Date date) {
		String sYear = "";
		if (date == null) {
			return sYear;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int year = cal.get(Calendar.YEAR);
		sYear = year + "";
		return sYear;
	}

	public static int getCurrentMonth() {
		Calendar calendar = new GregorianCalendar();
		return calendar.get(Calendar.MONTH) + 1;
	}

	public static int getMonth(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int month = cal.get(Calendar.MONTH) + 1;
		return month;
	}

	public static String getStringMonth(Date date) {
		String sMonth = "";
		if (date == null) {
			return sMonth;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int month = cal.get(Calendar.MONTH) + 1;
		if (month < 10) {
			sMonth = "0" + month;
		} else {
			sMonth = month + "";
		}
		return sMonth;
	}

	public static int getCurrentDay() {
		Calendar calendar = new GregorianCalendar();
		return calendar.get(Calendar.DAY_OF_MONTH);
	}

	public static int getDay(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int day = cal.get(Calendar.DAY_OF_MONTH);
		return day;
	}

	public static String getStringDay(Date date) {
		String sDay = "";
		if (date == null) {
			return sDay;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int day = cal.get(Calendar.DAY_OF_MONTH);
		if (day < 10) {
			sDay = "0" + day;
		} else {
			sDay = day + "";
		}
		return sDay;
	}

	public static int getCurrentHour() {
		Calendar calendar = new GregorianCalendar();
		return calendar.get(Calendar.HOUR_OF_DAY);
	}

	public static int getHour(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int hour = cal.get(Calendar.HOUR_OF_DAY);
		return hour;
	}

	public static int getMinute(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int minute = cal.get(Calendar.MINUTE);
		return minute;
	}

	public static int getSecond(Date date) {
		if (date == null) {
			return 0;
		}
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		int second = cal.get(Calendar.SECOND);
		return second;
	}

	public static Date createDate(int year, int month, int dayOfMonth, int hour) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, month - 1, dayOfMonth, hour, 0, 0);
		return cal.getTime();
	}

	public static Date createDate(int year, int month, int dayOfMonth, int hour, int minute, int second) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, month - 1, dayOfMonth, hour, minute, second);
		return cal.getTime();
	}

	public static Date createDate(int year, int month, int dayOfMonth) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, month - 1, dayOfMonth);
		return cal.getTime();
	}

	public static Date createDates(int year, int month) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, month - 1, 1);
		return cal.getTime();
	}
	public static Date firstDateOfMonth(int month, int year) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, month - 1, 1);
		return handleSubmit(cal.getTime());
	}

	public static Date firstDateOfYear(int year) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, 0, 1, 0, 0, 0);
		return cal.getTime();
	}

	/**
	 * Hàm trả về ngày cuối cùng trong năm được truyền vào
	 *
	 * @return Date
	 */
	public static Date createLastDate(String year) {
		String dateDMY = "31/12/" + year;
		return stringToDate(dateDMY);
	}
	
	public static Date lastDateOfYear(int year) {
		Calendar cal = Calendar.getInstance();
		cal.set(year, 11, 31, 23, 59, 59);
		return cal.getTime();
	}

	public static Date getCurrentTime() {
		Calendar calendar = Calendar.getInstance();
		return calendar.getTime();
	}

	public static String getDayHourMinute(Date date) {
		DateTimeUtils.getStringMonth(date);
		DateTimeUtils.getStringDay(date);
		DateTimeUtils.getStringYear(date);
		String hourminute = ":" + String.valueOf(DateTimeUtils.getHour(date));
		if (DateTimeUtils.getHour(date) == 0) {
			hourminute = "";
		}
		String targetString = DateTimeUtils.getStringDay(date) + "/" + DateTimeUtils.getStringMonth(date) + "/"
				+ DateTimeUtils.getStringYear(date) + hourminute;
		return targetString;
	}

	/**
	 * Tạo danh sách năm từ năm bắt đầu tới năm hiện tại. Nếu năm bắt đầu lớn hơn
	 * năm hiện tại của hệ thống thì trả về năm hiện tại.
	 *
	 * @param startYear - năm bắt đầu của danh sách
	 * @return List - danh sách năm
	 */
	public static final List<Integer> createYearList(int startYear) {
		List<Integer> result = new Vector<>();
		Date currentDate = new Date();
		int currentYear = DateTimeUtils.getYear(currentDate);
		if (currentYear < startYear) {
			result.add(currentYear);
		} else {
			for (int i = startYear; i <= currentYear + 1; i++) {
				result.add(i);
			}
		}
		return result;
	}

	/**
	 * @param date
	 * @param month
	 * @return
	 */
	public static Date addDayByMonth(Date date, short month) {
		String rets = "";

		int thang = DateTimeUtils.getMonth(date) + month;
		int phanle = thang % 12;
		int phannguyen = thang / 12;

		rets = rets + String.valueOf(DateTimeUtils.getDay(date));
		rets = rets + "/";
		rets = rets + String.valueOf(phanle == 0 ? 12 : phanle);
		rets = rets + "/";
		rets = rets + String.valueOf(DateTimeUtils.getYear(date) + (phanle == 0 ? phannguyen - 1 : phannguyen));

		return DateTimeUtils.stringToDate(rets);
	}

	/**
	 * Hàm trả ra ngày trước của ngày truyền vào, ví dụ ngày truyền vào là
	 * 12/10/2009 thì kết quả trả ra là ngày 11/10/2009
	 *
	 * @param date
	 * @return
	 */
	public static Date getDayBefore(Date date) {
		long MILLIS_IN_A_DAY = 1000L * 60L * 60L * 24L;

		Date dayBefore = new Date(date.getTime() - MILLIS_IN_A_DAY);
		return dayBefore;
	}

	/**
	 * Lấy ngày nằm sau.
	 *
	 * @param ngay   Ngày tính làm mốc.
	 * @param soNgay Số ngày nằm sau.
	 *
	 * @return Ngày cảnh báo nằm sau.
	 *
	 */
	public static Date getDayAfter(Date ngay, int soNgay) {
		GregorianCalendar gc = new GregorianCalendar();
		gc.setTime(ngay);
		gc.add(Calendar.DAY_OF_YEAR, soNgay);
		Date dayAfter = gc.getTime();

		return dayAfter;
	}

	/**
	 * Lấy ngày nằm trước.
	 *
	 * @param ngay   Ngày tính làm mốc.
	 * @param soNgay Số ngày cảnh báo sau.
	 *
	 * @return Ngày cảnh báo sau nhỏ nhất.
	 *
	 */
	public static Date getDayBefore(Date ngay, int soNgay) {
		GregorianCalendar gcSau = new GregorianCalendar();
		gcSau.setTime(ngay);
		gcSau.add(Calendar.DAY_OF_YEAR, -soNgay);
		Date dayBefore = gcSau.getTime();

		return dayBefore;
	}

	/**
	 * Hàm compare 2 ngày với nhau, chỉ tính ngày, không tính tới giờ, phút, giây
	 *
	 * @param day1
	 * @param day2
	 * @return 0 nếu bằng nhau, 1 nếu day1 > day2, -1 nếu day1 < day2
	 *
	 */
	public static int compare2Date(Date date1, Date date2) {
		if (getYear(date1) > getYear(date2)) {
			return 1;
		} else if (getYear(date1) < getYear(date2)) {
			return -1;
		}

		if (getMonth(date1) > getMonth(date2)) {
			return 1;
		} else if (getMonth(date1) < getMonth(date2)) {
			return -1;
		}

		if (getDay(date1) > getDay(date2)) {
			return 1;
		} else if (getDay(date1) < getDay(date2)) {
			return -1;
		}

		return 0;
	}

	/**
	 * Hàm compare 2 ngày với nhau, tính đến cả giờ phút giây
	 *
	 * @param day1
	 * @param day2
	 * @return 0 nếu bằng nhau, 1 nếu day1 > day2, -1 nếu day1 < day2
	 *
	 */
	public static int compare2DateTime(Date date1, Date date2) {
		if (getYear(date1) > getYear(date2)) {
			return 1;
		} else if (getYear(date1) < getYear(date2)) {
			return -1;
		}

		if (getMonth(date1) > getMonth(date2)) {
			return 1;
		} else if (getMonth(date1) < getMonth(date2)) {
			return -1;
		}

		if (getDay(date1) > getDay(date2)) {
			return 1;
		} else if (getDay(date1) < getDay(date2)) {
			return -1;
		}

		if (getHour(date1) > getHour(date2)) {
			return 1;
		} else if (getHour(date1) < getHour(date2)) {
			return -1;
		}

		if (getMinute(date1) > getMinute(date2)) {
			return 1;
		} else if (getMinute(date1) < getMinute(date2)) {
			return -1;
		}

		if (getSecond(date1) > getSecond(date2)) {
			return 1;
		} else if (getSecond(date1) < getSecond(date2)) {
			return -1;
		}
		return 0;
	}

	/**
	 * Hàm so sánh hai ngày, chỉ so sánh tháng và năm
	 *
	 * @param date1
	 * @param date2
	 * @return 0 nếu bằng nhau, < 0 nếu date1 < date2, > 0 nếu date1 > date2
	 */
	public static int compare2Month(Date date1, Date date2) {
		Calendar cal1 = Calendar.getInstance();
		cal1.setTime(date1);
		Calendar cal2 = Calendar.getInstance();
		cal2.setTime(date2);
		int y1 = cal1.get(Calendar.YEAR);
		int y2 = cal2.get(Calendar.YEAR);
		int m1 = cal1.get(Calendar.MONTH);
		int m2 = cal2.get(Calendar.MONTH);
		if (y1 != y2) {
			return y1 - y2;
		}
		return m1 - m2;
	}

	/**
	 * Hàm lấy số ngày giữa hai thời điểm d1 và d2
	 *
	 * @param d1
	 * @param d2
	 * @return
	 *
	 */
	public static long daysBetween2Date(Date d1, Date d2) {
		Calendar c1 = Calendar.getInstance();
		c1.setTime(d1);
		c1.set(Calendar.HOUR_OF_DAY, 0);

		Calendar c2 = Calendar.getInstance();
		c2.setTime(d2);
		c2.set(Calendar.HOUR_OF_DAY, 0);

		long ONE_HOUR = 60 * 60 * 1000L;
		// long day = ((d2.getTime() - d1.getTime() + ONE_HOUR) / (ONE_HOUR *
		// 24));
		long day = (c2.getTimeInMillis() - c1.getTimeInMillis()) / (ONE_HOUR * 24);
		return day < 0 ? -day : day;

	}

	/**
	 * Hàm lấy số tháng giữa hai khoảng thời gian d1 và d2
	 *
	 * @param d1
	 * @param d2
	 * @return
	 */
	public static int laySoThangGiuaHaiThoiDiem(Date d1, Date d2) {
		int ret = getMonth(d2) + getYear(d2) * 12 - (getMonth(d1) + getYear(d1) * 12);
		return ret;
	}

	/**
	 * Hàm xử lý ngày khi tăng hoặc giảm tháng.
	 *
	 * @param Thang là tháng cần tính ngày
	 * @param Nam   năm cần tính ngày
	 * @param ngay  là ngày của tháng được tính
	 * @return ngày trong Nam và Thang
	 */
	public static int layDay(int thang, int nam, int ngay) {

		Date ngayGoc = DateTimeUtils.stringToDate("1/" + String.valueOf(thang) + "/" + String.valueOf(nam));

		int day = DateTimeUtils.daysInMonth(ngayGoc);

		if (ngay <= day) {
			return ngay;
		} else {
			return day;
		}
	}

	/**
	 * Lấy ngày đầu tiên của tháng
	 */
	public static Date getFirstDayOfMonth(Date date) {
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.set(Calendar.DAY_OF_MONTH, 1);
		cal.set(Calendar.AM_PM, Calendar.AM);
		cal.set(Calendar.HOUR, 11);
		cal.set(Calendar.MINUTE, 59);
		cal.set(Calendar.SECOND, 59);
		return cal.getTime();
	}

	/**
	 * Lấy ngày đầu tiên của tháng theo thang nam nhap vao
	 *
	 * @author hieuhc
	 */
	public static Date getFirstDayOfMonthByMonthAndYear(int month, int year) {
		return createDates((short) year, (short) month);
	}

	public static Date getSameDayLastYear(Date date) {
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.add(Calendar.YEAR, -1);
		return cal.getTime();
	}

	/**
	 * Lấy ngày cuối cùng của tháng
	 */
	public static Date getLastDayOfMonth(Date date) {
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.set(Calendar.DAY_OF_MONTH, cal.getActualMaximum(Calendar.DAY_OF_MONTH));
		cal.set(Calendar.AM_PM, Calendar.PM);
		cal.set(Calendar.HOUR, 11);
		cal.set(Calendar.MINUTE, 59);
		cal.set(Calendar.SECOND, 59);
		return cal.getTime();
	}

	/**
	 * Lấy ngày cuối cùng của tháng theo thang nam nhap vao
	 *
	 * @author hieuhc
	 */
	public static Date lastDateOfMonth(int month, int year) {
		return getLastDayOfMonth(createDates((short) year, (short) month));
	}

	/**
	 * Lấy ngày đầu tiên của quý
	 *
	 * @author pmdn_tutm3
	 * @Time: Mar 6, 2013
	 * @param quarter
	 * @param year
	 * @return
	 */
	public static Date firstDayOfQuarter(int quarter, int year) {
		int month = quarter * 3 - 2;
		return createDates((short) year, (short) month);
	}

	/**
	 * @Description: Lấy ngày đầu tiên của 6 tháng đầu năm hoặc cuối năm
	 * @Author: pmdn_ducln1
	 * @Time: Apr 4, 2013
	 * @param quarter
	 * @param year
	 * @return
	 */
	public static Date getFirstDayOfSixMonth(int sixMonth, int year) {
		int month = sixMonth * 6 - 5;
		return createDates((short) year, (short) month);
	}

	/**
	 * @Description: Lấy ngày đầu tiên của năm
	 * @Author: pmdn_ducln1
	 * @Time: Apr 4, 2013
	 * @param year
	 * @return
	 */
	public static Date getFirstDayOfYear(int year) {
		return getFirstDayOfMonth(createDates(year, 1));
	}

	/**
	 * Lấy ngày cuối cùng của quý
	 *
	 * @author pmdn_tutm3
	 * @Time: Mar 6, 2013
	 * @param quarter
	 * @param year
	 * @return
	 */
	public static Date getLastDayOfQuarter(int quarter, int year) {
		int month = quarter * 3;
		return getLastDayOfMonth(createDates((short) year, (short) month));
	}

	/**
	 * Lấy ngày cuối cùng của 6 tháng đầu năm hoặc cuối năm
	 *
	 * @author ducln1
	 * @Time: Mar 6, 2013
	 * @param quarter
	 * @param year
	 * @return
	 */
	public static Date getLastDayOfSixMonth(int sixMonth, int year) {
		int month = sixMonth * 6;
		return getLastDayOfMonth(createDates((short) year, (short) month));
	}

	/**
	 * Lấy ngày cuối cùng của năm
	 *
	 * @author ducln1
	 * @Time: Mar 6, 2013
	 * @param quarter
	 * @param year
	 * @return
	 */
	public static Date getLastDayOfYear(int year) {
		return getLastDayOfMonth(createDates((short) year, (short) 12));
	}

	/**
	 * Ngày tháng theo định dạng
	 *
	 * @author pmdn_tutm3
	 * @Time: Mar 11, 2013
	 * @param date
	 * @param format
	 * @return
	 */
	public static String formatDate(Date date, String format) {
		if (date == null) {
			return "";
		}
		SimpleDateFormat sf = new SimpleDateFormat(format);
		return sf.format(date);
	}

	/**
	 * Đếm số ngày làm việc
	 *
	 * @author hieuhc
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	public static int countWorkingDay(Date startDate, Date endDate) {
		Calendar startCal;
		Calendar endCal;
		startCal = Calendar.getInstance();
		startCal.setTime(startDate);
		endCal = Calendar.getInstance();
		endCal.setTime(endDate);
		int workDays = 0;
		// Trong cùng ngày. Nếu ko là thứ 7 + chủ nhật thì tính là 1 ngày.
		if (compare2Date(startDate, endDate) == 0) {
			if (startCal.get(Calendar.DAY_OF_WEEK) != Calendar.SATURDAY
					&& startCal.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY) {
				++workDays;
			}
		}
		/*
		 *
		 */
		if (startCal.getTimeInMillis() > endCal.getTimeInMillis()) {
			startCal.setTime(endDate);
			endCal.setTime(startDate);
		}
		do {

			if (startCal.get(Calendar.DAY_OF_WEEK) != Calendar.SATURDAY
					&& startCal.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY) {
				++workDays;
			}
			startCal.add(Calendar.DAY_OF_MONTH, 1);
		} while (compare2Date(endCal.getTime(), startCal.getTime()) >= 0);

		return workDays;
	}

	public static List<Date> lastDateOfQuaters(int quater, int year) {
		List<Date> rs = new ArrayList<>();
		int[][] monthOfYear = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 }, { 10, 11, 13 } };
		int[] monthOfQuater = monthOfYear[quater - 1];

		for (int month : monthOfQuater) {
			rs.add(DateTimeUtils.getLastDayOfMonth(DateTimeUtils.createDates(year, month)));
		}
		return rs;
	}
	public static Date lastDateOfQuater(int quarter, int year) {
		int month = getMonthFromQuarterly(quarter, 2);
		return lastDateOfMonth(month, year);
	}

	public static List<Date> lastDateOfYears(int year) {
		List<Date> rs = new ArrayList<>();
		for (int i = 1; i <= 12; i++) {
			rs.add(DateTimeUtils.getLastDayOfMonth(DateTimeUtils.createDates(year, i)));
		}
		return rs;
	}

	public static List<Date> lastDateBetweenMonth(int monthFrom, int monthTo, int year) {
		List<Date> rs = new ArrayList<>();
		for (int i = monthFrom; i <= monthTo; i++) {
			rs.add(DateTimeUtils.getLastDayOfMonth(DateTimeUtils.createDates(year, i)));
		}
		return rs;
	}

	public static final int PERIOD_YEAR = 0;
	public static final int PERIOD_SIX_MONTH_FIRST = 13;
	public static final int PERIOD_SIX_MONTH_SECOND = 14;

	public static final int PERIOD_QUATER_FIRST = 15;
	public static final int PERIOD_QUATER_SECOND = 16;
	public static final int PERIOD_QUATER_THIRD = 17;
	public static final int PERIOD_QUATER_FOURTH = 18;

	protected static final Map<Integer, String> MONTH_MAP = new LinkedHashMap<>();
	static {
		MONTH_MAP.put(1, "Tháng 1");
		MONTH_MAP.put(2, "Tháng 2");
		MONTH_MAP.put(3, "Tháng 3");
		MONTH_MAP.put(4, "Tháng 4");
		MONTH_MAP.put(5, "Tháng 5");
		MONTH_MAP.put(6, "Tháng 6");
		MONTH_MAP.put(7, "Tháng 7");
		MONTH_MAP.put(8, "Tháng 8");
		MONTH_MAP.put(9, "Tháng 9");
		MONTH_MAP.put(10, "Tháng 10");
		MONTH_MAP.put(11, "Tháng 11");
		MONTH_MAP.put(12, "Tháng 12");
	}

	protected static final Map<Integer, String> QUATER_MAP = new LinkedHashMap<>();
	static {
		QUATER_MAP.put(PERIOD_QUATER_FIRST, "Quý 1");
		QUATER_MAP.put(PERIOD_QUATER_SECOND, "Quý 2");
		QUATER_MAP.put(PERIOD_QUATER_THIRD, "Quý 3");
		QUATER_MAP.put(PERIOD_QUATER_FOURTH, "Quý 4");
	}

	protected static final Map<Integer, String> SIX_MONTH_MAP = new LinkedHashMap<>();
	static {
		SIX_MONTH_MAP.put(PERIOD_SIX_MONTH_FIRST, "6 tháng đầu năm");
		SIX_MONTH_MAP.put(PERIOD_SIX_MONTH_SECOND, "6 tháng cuối năm");
	}

	protected static final Map<Integer, String> YEAR_MAP = new LinkedHashMap<>();
	static {
		YEAR_MAP.put(PERIOD_YEAR, "Cả năm");
	}

	/**
	 * Hằng số lưu các kỳ
	 */
	public static final int YEAR = 5;
	public static final int SIX_MONTH = 4;
	public static final int QUATER = 3;
	public static final int MONTH = 2;

	/**
	 * Hàm xử lý lấy các kỳ con. Ví dụ: 6 tháng đầu năm thì lấy quý 1, quý 2 hoặc
	 * tháng 1->6
	 *
	 * @param month       : tháng cha
	 * @param childPeriod : kỳ con
	 * @return danh sách các tháng con
	 */
	public static List<Integer> getChildMonth(int month) {
		List<Integer> monthList = new Vector<>();
		if (month == PERIOD_YEAR) {
			monthList.add(PERIOD_SIX_MONTH_FIRST);
			monthList.add(PERIOD_SIX_MONTH_SECOND);
			monthList.add(PERIOD_QUATER_FIRST);
			monthList.add(PERIOD_QUATER_SECOND);
			monthList.add(PERIOD_QUATER_THIRD);
			monthList.add(PERIOD_QUATER_FOURTH);
			for (int i = 1; i <= 12; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_SIX_MONTH_FIRST) {
			monthList.add(PERIOD_QUATER_FIRST);
			monthList.add(PERIOD_QUATER_SECOND);
			for (int i = 1; i <= 6; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_SIX_MONTH_SECOND) {
			monthList.add(PERIOD_QUATER_THIRD);
			monthList.add(PERIOD_QUATER_FOURTH);
			for (int i = 7; i <= 12; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_FIRST) {
			for (int i = 1; i <= 3; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_SECOND) {
			for (int i = 4; i <= 6; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_THIRD) {
			for (int i = 7; i <= 9; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_FOURTH) {
			for (int i = 10; i <= 12; i++) {
				monthList.add(i);
			}
		}

		return monthList;
	}

	/**
	 *
	 * @param month       : tháng cha
	 * @param childPeriod : kỳ con
	 * @return danh sách các tháng con
	 */
	public static List<Integer> getChildMonth(int month, int period) {
		List<Integer> monthList = new Vector<>();
		if (month == PERIOD_YEAR) {
			if (period == SIX_MONTH) {
				monthList.add(PERIOD_SIX_MONTH_FIRST);
				monthList.add(PERIOD_SIX_MONTH_SECOND);
			} else if (period == QUATER) {
				monthList.add(PERIOD_QUATER_FIRST);
				monthList.add(PERIOD_QUATER_SECOND);
				monthList.add(PERIOD_QUATER_THIRD);
				monthList.add(PERIOD_QUATER_FOURTH);
			} else if (period == MONTH) {
				for (int i = 1; i <= 12; i++) {
					monthList.add(i);
				}
			}
		} else if (month == PERIOD_SIX_MONTH_FIRST) {
			if (period == QUATER) {
				monthList.add(PERIOD_QUATER_FIRST);
				monthList.add(PERIOD_QUATER_SECOND);
			} else if (period == MONTH) {
				for (int i = 1; i <= 6; i++) {
					monthList.add(i);
				}
			}
		} else if (month == PERIOD_SIX_MONTH_SECOND) {
			if (period == QUATER) {
				monthList.add(PERIOD_QUATER_THIRD);
				monthList.add(PERIOD_QUATER_FOURTH);
			} else if (period == MONTH) {
				for (int i = 7; i <= 12; i++) {
					monthList.add(i);
				}
			}
		} else if (month == PERIOD_QUATER_FIRST) {
			for (int i = 1; i <= 3; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_SECOND) {
			for (int i = 4; i <= 6; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_THIRD) {
			for (int i = 7; i <= 9; i++) {
				monthList.add(i);
			}
		} else if (month == PERIOD_QUATER_FOURTH) {
			for (int i = 10; i <= 12; i++) {
				monthList.add(i);
			}
		}

		return monthList;
	}

	public static Integer getQuater(Date date) {
		Integer month = DateTimeUtils.getMonth(date);
		if (1 <= month && month <= 3) {
			return PERIOD_QUATER_FIRST;
		} else if (4 <= month && month <= 6) {
			return PERIOD_QUATER_SECOND;
		} else if (7 <= month && month <= 9) {
			return PERIOD_QUATER_THIRD;
		} else if (10 <= month && month <= 12) {
			return PERIOD_QUATER_FOURTH;
		} else {
			return 0;
		}
	}

	public static Integer getSixMonth(Date date) {
		Integer month = DateTimeUtils.getMonth(date);
		if (1 <= month && month <= 6) {
			return PERIOD_SIX_MONTH_FIRST;
		} else if (7 <= month && month <= 12) {
			return PERIOD_SIX_MONTH_SECOND;
		} else {
			return 0;
		}
	}

	/**
	 * Lay ngay khong chua thoi gian
	 */
	public static Date getDateNotTime(Date date) {
		SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
		try {
			return sdf.parse(sdf.format(date));
		} catch (ParseException e) {
			log.info(e);
			return null;
		}
	}

	public static int getDayOfWeek(Date date) {
		Calendar calendar = Calendar.getInstance(timeZone());
		calendar.setTime(date);
		return calendar.get(Calendar.DAY_OF_WEEK);
	}

	public static Date converttoLocalTime(Date date) {

		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(date.getTime());
		int offset = TimeZone.getDefault().getOffset(date.getTime());
		calendar.add(Calendar.MILLISECOND, offset);
		return calendar.getTime();
	}

	public static Date convertDateFromStringPattern(String dateStr, String pattern) {

		Date _converted;
		try {
			_converted = new SimpleDateFormat(pattern).parse(dateStr);
		} catch (ParseException e) {
			log.info(e);
			_converted = null;
		}
		return _converted;
	}

	public static String convertDateToStringPattern(Date date, String pattern) {
		if (date == null)
			date = new Date();
		DateFormat dateFormat = new SimpleDateFormat(pattern);
		try {
			return dateFormat.format(date);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static Date getDateTomorrow() {

		Calendar c = Calendar.getInstance();
		c.setTime(new Date());
		c.add(Calendar.DATE, 1);
		return c.getTime();
	}

	public static Date getDayFirstDate(Date date) {
		Calendar first = Calendar.getInstance();
		first.setTime(date);
		first.add(Calendar.DAY_OF_WEEK, first.getFirstDayOfWeek() - first.get(Calendar.DAY_OF_WEEK));
		first.add(Calendar.DATE, 1);
		return first.getTime();
	}

	public static Date getDayLastDate(Date date) {
		Calendar first = Calendar.getInstance();
		first.setTime(date);
		first.add(Calendar.DAY_OF_WEEK, first.getFirstDayOfWeek() - first.get(Calendar.DAY_OF_WEEK));
		first.add(Calendar.DATE, 7);
		return first.getTime();
	}

	public static Date getTomorrow(Date today) {
		if (today == null) {
			return null;
		}
		Calendar c = Calendar.getInstance();
		c.setTime(today);
		c.add(Calendar.DATE, 1);
		return c.getTime();
	}
	
	public static Date getYesterday(Date today) {
		if (today == null) {
			return null;
		}
		Calendar c = Calendar.getInstance();
		c.setTime(today);
		c.add(Calendar.DATE, -1);
		return c.getTime();
	}

	public static Date handleSubmit(Date date) {
		return handleSubmit(date, Calendar.MILLISECOND, 0);
	}

	public static Date handleSubmit(Date date, int field, int amount) {
		return handleSubmit(date, field, amount, 0);
	}

	public static Calendar calendar(Date date) {
		Calendar c = Calendar.getInstance(timeZone());
		c.setTime(date);
		return c;
	}

	public static TimeZone timeZone() {
		return TimeZone.getTimeZone(TIME_ZONE_ID);
	}

	public static DateFormat dateFormat() {
		return dateFormat("dd/MM/yyyy");
	}

	public static DateFormat dateFormat(String format) {
		DateFormat formatter = new SimpleDateFormat(format);
		formatter.setTimeZone(timeZone());
		return formatter;
	}

	public static Date setDate(String date) {
		DateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
		formatter.setTimeZone(timeZone());
		try {
			return formatter.parse(date);
		} catch (ParseException e) {
			return null;
		}
	}

	public static Date handleSubmit(Date date, int field, int amount, int delta) {
		if (date == null) {
			return null;
		}
		DateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
		formatter.setTimeZone(timeZone());
		try {
			date = formatter.parse(formatter.format(date));
			Calendar time = Calendar.getInstance();
			time.setTime(date);
			time.add(field, amount);
			time.add(Calendar.MILLISECOND, delta);
			return time.getTime();
		} catch (ParseException e) {
			return null;
		}
	}

	/***
	 * get frMonth / toMonth from quarterly
	 *
	 * @param quarterly
	 * @param op        : 1-frMonth/ 2-toMonth
	 * @return
	 */
	public static int getMonthFromQuarterly(Integer quarterly, int op) {
		int toMonth = 0;
		int frMonth = 0;
		if (quarterly == null) {
			return 0;
		}
		if (quarterly.intValue() == 1) {
			toMonth = 1;
			frMonth = 3;
		}
		if (quarterly.intValue() == 2) {
			toMonth = 4;
			frMonth = 6;
		}
		if (quarterly.intValue() == 3) {
			toMonth = 7;
			frMonth = 9;
		}
		if (quarterly.intValue() == 4) {
			toMonth = 10;
			frMonth = 12;
		}
		if (op == 1) {
			return toMonth;
		}
		if (op == 2) {
			return frMonth;
		}
		return 0;
	}
	
	public static List<Integer> getMonthFromQuarter(int quarter) {
		List<Integer> months;
		switch (quarter) {
		case 1:
			months = Arrays.asList(1, 2, 3);
			break;
		case 2:
			months = Arrays.asList(4, 5, 6);
			break;
		case 3:
			months = Arrays.asList(7, 8, 9);
			break;
		case 4:
			months = Arrays.asList(10, 11, 12);
			break;
		default:
			months = Arrays.asList(1, 2, 3);
			break;
		}
		return months;
	}

	public static Date getLastDate(Date date1, Date date2) {
		if (date1 == null || date2 == null) {
			return null;
		}
		if (date1.getTime() > date2.getTime()) {
			return date1;
		}
		return date2;
	}

	public static long getTimeServer() {
		NTPUDPClient timeClient = new NTPUDPClient();
		try {
			timeClient.open();
			InetAddress inetAddress = InetAddress.getByName(TIME_SERVER);
			TimeInfo timeInfo = timeClient.getTime(inetAddress);
			long timeSV = timeInfo.getMessage().getTransmitTimeStamp().getTime();
			log.info("Time server info : " + new Date(timeSV));
			return timeSV;
		} catch (Exception e) {
			log.info("Time server error - " + e.getLocalizedMessage());
			return 0;
		} finally {
			timeClient.close();
		}
	}
	
	//type = 1 -> start date 
	//type = 2 -> end date
	@SuppressWarnings("deprecation")
	public static Date getDateByWeek(int week, int year, int type) {
		Date startDate = null;
		Date endDate = null;

		Calendar c = Calendar.getInstance(timeZone());
		if(year <= 0 ) {
			year = c.get(Calendar.YEAR);
		}
		
		c.set(Calendar.YEAR, year);
		if(week > 0) c.set(Calendar.WEEK_OF_YEAR, week);
		c.set(Calendar.DAY_OF_WEEK, Calendar.MONDAY);
		
		startDate = handleSubmit(c.getTime());
		
		c.setTime(startDate);
		c.add(Calendar.DATE, 7);
		c.add(Calendar.MILLISECOND, -1);
		endDate = c.getTime();
		
		log.info("Start date is : " + startDate);
		log.info("End date is : " + endDate);
		
		if (type == TYPE_START_DATE) return startDate;
		if (type == TYPE_END_DATE) return endDate;

		return null;
	}
	
	public static String getDateOfWeekString(Date date) {
	    return dayOfWeekMap.get(getDayOfWeek(date));
	}
	
	public static String getHourMinutes(Date date) {
		DateFormat df = new SimpleDateFormat("HH:mm");
		df.setTimeZone(timeZone());
		return df.format(date);
	}
	
	public static boolean isAmPM(Date date) {
		Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
		c.setTime(date);
		int am_pm = c.get(Calendar.AM_PM);
		log.info("AM is 0 AND PM is 1 : "+ am_pm);
		return am_pm == Calendar.AM;
	}
	
	public static int getWeekOfYear(Date date) {
		Calendar calendar = Calendar.getInstance(DateTimeUtils.timeZone());
		calendar.setTime(date);
		return calendar.get(Calendar.WEEK_OF_YEAR);
	}
	
	/**
	 * get date by date (thu hai/ba/tu) and week
	 * Monday of week is date ?
	 * @param dateTh
	 * @param week
	 * @return
	 */
	public static Date getDateByDateThAndWeek(int dateTh, int week, int year) {
		Calendar c = Calendar.getInstance(timeZone());
		if(year <= 0) year = c.get(Calendar.YEAR);
		c.set(Calendar.YEAR, year);
		c.set(Calendar.WEEK_OF_YEAR, week);
		c.set(Calendar.DAY_OF_WEEK, dateTh);
		if(Calendar.SUNDAY == dateTh) c.add(Calendar.DATE, 7); //kéo lui 7 ngày
		return c.getTime();
	}
	
	public static Date getEndDate(Date endDate) {
		if (endDate == null)
			return null;
		Calendar c = Calendar.getInstance(timeZone());
		c.setTime(endDate);
		c.set(Calendar.HOUR, 23);
		c.set(Calendar.MINUTE, 59);
		c.set(Calendar.SECOND, 59);
		return c.getTime();
	}

	public static Date getStartDate(Date startDate) {
		if (startDate == null)
			return null;
		Calendar c = Calendar.getInstance(timeZone());
		c.setTime(startDate);
		c.set(Calendar.HOUR, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		return c.getTime();
	}
}
