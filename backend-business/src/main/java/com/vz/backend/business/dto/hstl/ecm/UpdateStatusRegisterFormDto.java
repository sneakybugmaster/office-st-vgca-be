package com.vz.backend.business.dto.hstl.ecm;

import java.util.Date;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
public class UpdateStatusRegisterFormDto {
	private Date ngayXuLy;
	private String userNameXuLY;
	private String lyDo;
	private Integer status;
}
