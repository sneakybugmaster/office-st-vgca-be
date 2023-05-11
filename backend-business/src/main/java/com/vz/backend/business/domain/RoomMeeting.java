package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.*;

import com.vz.backend.business.dto.MeetingTimeDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.StringUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
@Entity
@Table(name = "ROOM_MEETING", schema = "vz", indexes = {
		@Index(name = "idx_roommeeting_id", columnList = "id")
})
@Getter
@Setter
@NoArgsConstructor
public class RoomMeeting extends BaseModel {

	@Column(name = "[name]")
	private String name;

	@Column(name = "address")
	private String address;

	@Column(name = "quantity", columnDefinition = "int default 1")
	private int quantity;

	@Column(name = "acreage", columnDefinition = "float default 1.0")
	private float acreage;

	@Column(name = "description")
	private String description;

	@Transient
	List<MeetingTimeDto> meetingTimes;

	public void valid() {

		//valid quantity
//		if(quantity <= 0 )
//			throw new RestExceptionHandler(Message.QUANTITY_ROOM_INVALID);

		// valid title
		if (StringUtils.isNullOrEmpty(this.getName())) {
			throw new RestExceptionHandler(Message.ROOM_NAME_INVALID);
		}
		BussinessCommon.validLengthData(this.getName(), "Tên phòng họp", 200);
		BussinessCommon.validLengthData(this.getAddress(), "Địa điểm", 200);
		BussinessCommon.validLengthData(this.getDescription(), "Ghi chú", 200);
	}
}
