package prosomo;

import java.lang.management.ManagementFactory;
import com.sun.management.OperatingSystemMXBean;

public class SystemLoadMonitor {
    OperatingSystemMXBean bean = (com.sun.management.OperatingSystemMXBean) ManagementFactory
            .getOperatingSystemMXBean();
    double cpuLoad() {
        double value = bean.getSystemCpuLoad();
        return value;
    }
}
