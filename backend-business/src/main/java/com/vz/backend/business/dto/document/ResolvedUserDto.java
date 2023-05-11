package com.vz.backend.business.dto.document;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.UserBasicDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class ResolvedUserDto extends UserBasicDto {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private boolean leaderShip;
	private String userInfo = "";
	private String content = "";
	
	public static List<ResolvedUserDto> convert(List<DocumentComment> cmts, List<ResolvedUserDto> dto) {
		Map<Long, ResolvedUserDto> map = new HashMap<>();
		dto.forEach(i -> {
			Long key = i.getId();
			if (!map.containsKey(key)) {
				map.put(key, i);
			}
		});

		List<ResolvedUserDto> rs = new ArrayList<>();
		cmts.forEach(i -> {
			Long key = i.getCreateBy();
			if (map.containsKey(key)) {
				ResolvedUserDto value = map.get(key);
				rs.add(new ResolvedUserDto(value.isLeaderShip(), value.getUserInfo(), i.getComment()));
			}
		});

		return rs;
	}

	public ResolvedUserDto(User user, boolean leaderShip) {
		this.leaderShip = leaderShip;
		super.setId(user.getId());
		this.userInfo = user.getFullName() + " - " + user.getPositionModel().getName() + " - "
				+ user.getOrgModel().getName();
	}
}
