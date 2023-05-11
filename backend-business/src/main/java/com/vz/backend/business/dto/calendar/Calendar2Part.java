package com.vz.backend.business.dto.calendar;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jsoup.Jsoup;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.Utils;

import lombok.Getter;

@Getter
@Setter
@NoArgsConstructor
public class Calendar2Part {
	public static final String BREAK_LINE = "##";
	private static final Map<Integer, String> dayOfWeekMap;
	static {
		Map<Integer, String> aMap = new HashMap<>();
		aMap.put(Calendar.SUNDAY, "Chủ nhật");
		aMap.put(Calendar.MONDAY, "Thứ hai");
		aMap.put(Calendar.TUESDAY, "Thứ ba");
		aMap.put(Calendar.WEDNESDAY, "Thứ tư");
		aMap.put(Calendar.THURSDAY, "Thứ năm");
		aMap.put(Calendar.FRIDAY, "Thứ sáu");
		aMap.put(Calendar.SATURDAY, "Thứ bảy");
		dayOfWeekMap = Collections.unmodifiableMap(aMap);
	}

	private boolean showDateStr = true;

	public void hideDateStr() {
		this.showDateStr = false;
	}

	public String getDateStr() {
		if (!showDateStr) {
			return "";
		}
		return dateStr2;
	}

	private String dateStr2;
	private String content;
	private String ingredient;
	private String time;
	private String address;
	private Date date;

	public Calendar2Part(Calendar2 cal) {
		DateFormat formatDate = DateTimeUtils.dateFormat();
		DateFormat formatHour = DateTimeUtils.dateFormat("HH:mm");
		Calendar calendar = DateTimeUtils.calendar(cal.getStartTime());
		this.date = calendar.getTime();
		this.dateStr2 = dayOfWeekMap.get(calendar.get(Calendar.DAY_OF_WEEK)) + BREAK_LINE
				+ formatDate.format(this.date);
		this.content = removeTag(Utils.coalesce(cal.getDescription(), ""));
		this.ingredient = Utils.coalesce(cal.getParticipantsGuest(), "");
		this.time = formatHour.format(this.date);
		this.address = Utils.coalesce(cal.getAddress(), "");
	}

	private String removeTag(String content) {
		if (content == null) {
			return content;
		}
		return Jsoup.parse(content).text();
	}
}
