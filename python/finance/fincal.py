def get_present_value(future_value, rate, n_periods):
    return future_value / (1 + rate)**n_periods
