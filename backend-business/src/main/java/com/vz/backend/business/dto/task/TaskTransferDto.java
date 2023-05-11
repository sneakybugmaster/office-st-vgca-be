package com.vz.backend.business.dto.task;

import java.util.List;

import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.business.domain.TaskHistory;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TaskTransferDto {
	private List<TaskExecute> taskExecutes;
	private TaskHistory taskHistory;
	private Long nodeId;

	/**
	 * Validation
	 * @param taskId
	 */
	public void valids(Long taskId, boolean isCheckMain) {
		// empty data
		if (taskId == null || this.nodeId == null || BussinessCommon.isEmptyList(this.taskExecutes))
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);

		// valid data
		this.taskExecutes.forEach(i -> i.valids());

		if (isCheckMain) {
			// One main is transferred
			long main = this.taskExecutes.stream().filter(i -> Boolean.TRUE.equals(i.getIsExcute()))
					.map(TaskExecute::getUserId).count();
			if (main == 0) {
				throw new RestExceptionHandler(Message.NO_MAIN_TRANSFER_TASK);
			}

			if (main != 1) {
				throw new RestExceptionHandler(Message.ONE_MAIN_TRANSFER_TASK);
			}
		}
		
		if (this.taskHistory != null) {
			this.taskHistory.valids();
		}
	}
}
